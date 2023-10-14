-module(msp).

-behaviour(gen_server).

-export([start_link/1]).
-export([state/1, start_listening/3, connect/4, open_stream/6, append_stream/5]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

state(PID) ->
    gen_server:call(PID, get_state).

start_listening(PID, ConnectionPID, StreamPID) ->
    gen_server:cast(PID, {start_listening, ConnectionPID, StreamPID}).

connect(PID, IP, Port, OpenListener) ->
    % The connection itself times out after 5000ms, so the call should
    % definitely reply within 6000ms, unless we are inundated by messages.
    gen_server:call(PID, {connect, IP, Port, OpenListener}, 6000).

% TODO: remove the first payload, and wait for a separate call?
open_stream(PID, IP, Port, HandlerPID, FirstPayload, FirstYield) ->
    gen_server:cast(PID, {open_stream, IP, Port, HandlerPID, FirstPayload, FirstYield}).

append_stream(PID, IP, Port, Payload, Yield) ->
    gen_server:cast(PID, {append_stream, IP, Port, Payload, Yield}).

-type endpoint() :: {inet:ip_address(), inet:port_number()}.

-record(msp_datagram, {index :: integer(),
                       yield :: boolean(),
                       payload :: binary()}).
-record(msp_stream, {data = [] :: [#msp_datagram{}],
                     next_index :: integer(),
                     has_control :: boolean(),
                     event_listener :: pid()}).
-record(msp_peer, {streams = #{} :: #{integer() => #msp_stream{}},
                   stream_open_listener :: pid()}).
-record(msp_pending_peer, {address :: endpoint(),
                           timeout_ref :: reference(),
                           timeout_count = 0 :: integer(),
                           attempt_max :: integer(),
                           caller :: gen_server:from(),
                           stream_open_listener :: pid()}).
-record(msp_state, {socket :: gen_udp:socket(),
                    pending_peers = [] :: [#msp_pending_peer{}],
                    peers = #{} :: #{endpoint() => #msp_peer{}},
                    listeners = none :: none | {pid(), pid()}}).

start_link(Port) ->
    gen_server:start_link(?MODULE, Port, []).

init(Port) ->
    {ok, Socket} = gen_udp:open(Port, [{active, once}]),
    {ok, #msp_state{socket = Socket}}.

handle_call(get_state, _, State) ->
    {reply, State, State};
handle_call({connect, IP, Port, OpenListener}, Caller, State) ->
    NewState = do_connect(State, IP, Port, Caller, OpenListener),
    {noreply, NewState}.

handle_cast({start_listening, ConnectionPID, StreamPID}, State) ->
    NewState = State#msp_state{listeners = {ConnectionPID, StreamPID}},
    {noreply, NewState}.

handle_info({udp, Socket, IP, Port, Data}, State = #msp_state{socket = Socket}) ->
    NewState = do_receive(State, IP, Port, Data),
    inet:setopts(Socket, [{active, once}]),
    {noreply, NewState};
handle_info({timeout, TRef, connect}, State) ->
    NewState = do_connect_timeout(State, TRef),
    {noreply, NewState}.

do_receive(State = #msp_state{listeners = none}, IP, Port, "msp connect") ->
    io:format("~s:~w tried to connect. Discarding.~n", [inet:ntoa(IP), Port]),
    State;
do_receive(State, IP, Port, "msp connect") ->
    do_accept(State, IP, Port);
do_receive(State, IP, Port, "connection accepted") ->
    do_connection_success(State, IP, Port);
do_receive(State, IP, Port, Data) ->
    io:format("~s:~w sent datagram: ~s~n", [inet:ntoa(IP), Port, Data]),
    State.

do_accept(State, IP, Port) ->
    % Accept the connection regardless of whether they were already connected,
    % the previous accept message may have been dropped.
    ok = gen_udp:send(State#msp_state.socket, IP, Port, "connection accepted"),
    case maps:is_key({IP, Port}, State#msp_state.peers) of
        true ->
            State;
        false ->
            % Genuinely connecting for the first time, notify the listening
            % process.
            {ConnectListener, OpenListener} = State#msp_state.listeners,
            erlang:send(ConnectListener, {msp_connect, IP, Port}),

            % Now add them as a peer and continue with msp business.
            Peer = #msp_peer{stream_open_listener = OpenListener},
            NewPeers = maps:put({IP, Port}, Peer, State#msp_state.peers),
            State#msp_state{peers = NewPeers}
    end.

do_connect(State, IP, Port, Caller, OpenListener) ->
    ok = gen_udp:send(State#msp_state.socket, IP, Port, "msp connect"),
    TRef = erlang:start_timer(1000, self(), connect),
    Pending = #msp_pending_peer{address = {IP, Port},
                                timeout_ref = TRef,
                                attempt_max = 5,
                                caller = Caller,
                                stream_open_listener = OpenListener},
    NewPending = [Pending | State#msp_state.pending_peers],
    State#msp_state{pending_peers = NewPending}.

do_connect_timeout(State, TRef) ->
    PendingPeers = State#msp_state.pending_peers,
    case lists:keytake(TRef, #msp_pending_peer.timeout_ref, PendingPeers) of
        {value, Pending, NewPending} ->
            NewState = State#msp_state{pending_peers = NewPending},
            do_connect_timeout2(NewState, Pending);
        false ->
            State
    end.

do_connect_timeout2(State, PrevPending) ->
    Timeouts = PrevPending#msp_pending_peer.timeout_count + 1,
    case Timeouts < PrevPending#msp_pending_peer.attempt_max of
        true ->
            {IP, Port} = PrevPending#msp_pending_peer.address,
            ok = gen_udp:send(State#msp_state.socket, IP, Port, "msp connect"),

            TRef = erlang:start_timer(1000, self(), connect),

            Pending = PrevPending#msp_pending_peer{timeout_ref = TRef,
                                                   timeout_count = Timeouts},
            NewPending = [Pending | State#msp_state.pending_peers],
            State#msp_state{pending_peers = NewPending};
        false ->
            Caller = PrevPending#msp_pending_peer.caller,
            gen_server:reply(Caller, {error, timeout}),
            State
    end.

do_connection_success(State, IP, Port) ->
    PendingPeers = State#msp_state.pending_peers,
    case lists:keytake({IP, Port}, #msp_pending_peer.address, PendingPeers) of
        {value, Pending, NewPending} ->
            gen_server:reply(Pending#msp_pending_peer.caller, ok),
            TRef = Pending#msp_pending_peer.timeout_ref,
            erlang:cancel_timer(TRef, [{async, true}, {info, false}]),

            OpenListener = Pending#msp_pending_peer.stream_open_listener,
            Peer = #msp_peer{stream_open_listener = OpenListener},
            NewPeers = maps:put({IP, Port}, Peer, State#msp_state.peers),
            State#msp_state{pending_peers = NewPending,
                            peers = NewPeers};
        false ->
            State
    end.

