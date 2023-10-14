-module(msp).

-behaviour(gen_server).

-export([start_link/1]).
-export([state/1, start_listening/3, connect/4, open_stream/6, append_stream/6]).
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
    gen_server:call(PID, {open_stream, IP, Port, HandlerPID, FirstPayload, FirstYield}).

append_stream(PID, IP, Port, StreamID, Payload, Yield) ->
    gen_server:call(PID, {append_stream, IP, Port, StreamID, Payload, Yield}).

-type endpoint() :: {inet:ip_address(), inet:port_number()}.

-record(msp_datagram, {index :: integer(),
                       yield :: boolean(),
                       % TODO: use binaries? IO lists?
                       payload :: string()}).
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
    {noreply, NewState};
handle_call({open_stream, IP, Port, HandlerPID, FirstPayload, FirstYield}, Caller, State) ->
    NewState = do_open_stream(State, IP, Port, HandlerPID, FirstPayload,
                              FirstYield, Caller),
    {noreply, NewState};
handle_call({append_stream, IP, Port, StreamID, Payload, Yield}, Caller, State) ->
    NewState = do_append_stream(State, IP, Port, StreamID, Payload, Yield, Caller),
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
do_receive(State, IP, Port, [0 | Data]) ->
    do_receive2(State, IP, Port, false, Data);
do_receive(State, IP, Port, [1 | Data]) ->
    do_receive2(State, IP, Port, true, Data);
do_receive(State, IP, Port, Data) ->
    io:format("~s:~w sent datagram: ~s~n", [inet:ntoa(IP), Port, Data]),
    State.

do_receive2(State, IP, Port, Yield, Data) ->
    case decode_int(Data) of
        {ok, StreamID, Data2} ->
            do_receive3(State, IP, Port, Yield, StreamID, Data2);
        error ->
            State
    end.

do_receive3(State, IP, Port, Yield, StreamID, Data2) ->
    case decode_int(Data2) of
        {ok, Index, Payload} ->
            do_handle_datagram(State, IP, Port, StreamID, Index, Yield, Payload);
        error ->
            State
    end.

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

do_open_stream(State, IP, Port, HandlerPID, FirstPayload, FirstYield, Caller) ->
    case maps:find({IP, Port}, State#msp_state.peers) of
        {ok, Peer} ->
            % The reply only requires the ID, so send it quickly, before we
            % bother with any network nonsense.
            % TODO: If we have to concede streams then this id might not turn
            % out to be correct. Should we use even/odd stream IDs just to make
            % all this stuff simple?
            StreamID = choose_stream_id(Peer),
            gen_server:reply(Caller, {ok, StreamID}),

            Stream = #msp_stream{next_index = 0,
                                 has_control = true,
                                 event_listener = HandlerPID},
            % Skip all of the guards of `append_stream`, since we already
            % passed or constructed the cases ourselves, just send the datagram
            % and rebuild.
            build_send_datagram(State, IP, Port, StreamID, FirstPayload,
                                FirstYield, Peer, Stream);
        error ->
            gen_server:reply(Caller, {error, not_connected})
    end.

choose_stream_id(#msp_peer{streams = Streams}) ->
    Attempt1 = maps:size(Streams),
    choose_stream_id(Streams, Attempt1).

choose_stream_id(Streams, Attempt) ->
    case maps:is_key(Attempt, Streams) of
        true ->
            choose_stream_id(Streams, Attempt + 1);
        false ->
            Attempt
    end.

do_append_stream(State, IP, Port, StreamID, Payload, Yield, Caller) ->
    case maps:find({IP, Port}, State#msp_state.peers) of
        {ok, Peer} ->
            do_append_stream2(State, IP, Port, StreamID, Payload, Yield,
                              Caller, Peer);
        error ->
            gen_server:reply(Caller, {error, not_connected}),
            State
    end.

do_append_stream2(State, IP, Port, StreamID, Payload, Yield, Caller, Peer) ->
    case maps:find(StreamID, Peer#msp_peer.streams) of
        {ok, Stream} ->
            do_append_stream3(State, IP, Port, StreamID, Payload, Yield,
                              Caller, Peer, Stream);
        error ->
            gen_server:reply(Caller, {error, no_stream}),
            State
    end.

do_append_stream3(State, _, _, _, _, _, Caller, _, #msp_stream{has_control = false}) ->
    gen_server:reply(Caller, {error, not_our_turn}),
    State;
do_append_stream3(State, IP, Port, StreamID, Payload, Yield, Caller, Peer, Stream) ->
    Index = Stream#msp_stream.next_index,
    gen_server:reply(Caller, {ok, Index}),

    build_send_datagram(State, IP, Port, StreamID, Payload, Yield, Peer, Stream).

build_send_datagram(State, IP, Port, StreamID, Payload, Yield, Peer, Stream) ->
    Index = Stream#msp_stream.next_index,
    Datagram = #msp_datagram{index = Index,
                             yield = Yield,
                             payload = Payload},
    send_datagram(State, IP, Port, StreamID, Datagram),

    % Add the tracking info for this new stream.
    Data = [Datagram | Stream#msp_stream.data],
    NewStream = Stream#msp_stream{data = Data,
                                  next_index = Index + 1,
                                  has_control = not Yield},
    NewStreams = maps:put(StreamID, NewStream, Peer#msp_peer.streams),
    NewPeer = Peer#msp_peer{streams = NewStreams},
    NewPeers = maps:put({IP, Port}, NewPeer, State#msp_state.peers),
    State#msp_state{peers = NewPeers}.


send_datagram(State, IP, Port, StreamID, DatagramInfo) ->
    YieldByte = case DatagramInfo#msp_datagram.yield of
                    true  -> 1;
                    false -> 0
                end,
    Data = [YieldByte,
            encode_int(StreamID),
            encode_int(DatagramInfo#msp_datagram.index),
            DatagramInfo#msp_datagram.payload],
    ok = gen_udp:send(State#msp_state.socket, IP, Port, Data),
    ok.

encode_int(N) ->
    encode_int(N, []).

% 7 bits at a time, little endian.
encode_int(N, Acc) when 0 =< N, N < 128 ->
    lists:reverse([N | Acc]);
encode_int(N, Acc) when N >= 128 ->
    LowBits = N band 127,
    HighBits = N bsr 7,
    encode_int(HighBits, [128 bor LowBits | Acc]).

decode_int(Bytes) ->
    decode_int(Bytes, 0, 0).

decode_int([Byte | Rest], N, Shift) ->
    Bits = Byte band 127,
    NewN = N bor (Bits bsl Shift),
    case Byte < 128 of
        true  -> {ok, NewN, Rest};
        false -> decode_int(Rest, NewN, Shift + 7)
    end;
decode_int([], _, _) ->
    error.

do_handle_datagram(State, IP, Port, StreamID, Index, Yield, Payload) ->
    case maps:find({IP, Port}, State#msp_state.peers) of
        {ok, Peer} ->
            do_handle_datagram2(State, IP, Port, Peer, StreamID, Index, Yield, Payload);
        error ->
            State
    end.

do_handle_datagram2(State, IP, Port, Peer, StreamID, 0, Yield, Payload) ->
    case maps:is_key(StreamID, Peer#msp_peer.streams) of
        true ->
            io:format("~s:~w may have opened a stream that we already opened. "
                      "Discarding.~n", [inet:ntoa(IP), Port]),
            State;
        false ->
            StreamListener = Peer#msp_peer.stream_open_listener,
            erlang:send(StreamListener, {msp_datagram, IP, Port, StreamID, 0,
                                         Yield, Payload}),
            % TODO: Separate the opening notification from the actual flushing,
            % until an external process explicitly registers to the stream?
            Stream = #msp_stream{next_index = 1,
                                 has_control = Yield,
                                 event_listener = StreamListener},

            NewStreams = maps:put(StreamID, Stream, Peer#msp_peer.streams),
            NewPeer = Peer#msp_peer{streams = NewStreams},
            NewPeers = maps:put({IP, Port}, NewPeer, State#msp_state.peers),
            State#msp_state{peers = NewPeers}
    end;
do_handle_datagram2(State, IP, Port, Peer, StreamID, Index, Yield, Payload) ->
    case maps:find(StreamID, Peer#msp_peer.streams) of
        {ok, Stream} ->
            do_handle_datagram3(State, IP, Port, Peer, StreamID, Stream, Index,
                                Yield, Payload);
        error ->
            State
    end.

do_handle_datagram3(State, IP, Port, Peer, StreamID, Stream, Index, Yield, Payload) ->
    Expected = Stream#msp_stream.next_index,
    case (Index == Expected) and not Stream#msp_stream.has_control of
        true ->
            StreamListener = Stream#msp_stream.event_listener,
            erlang:send(StreamListener, {msp_datagram, IP, Port, StreamID,
                                         Index, Yield, Payload}),

            NewStream = Stream#msp_stream{next_index = Index + 1,
                                          has_control = Yield},
            NewStreams = maps:put(StreamID, NewStream, Peer#msp_peer.streams),
            NewPeer = Peer#msp_peer{streams = NewStreams},
            NewPeers = maps:put({IP, Port}, NewPeer, State#msp_state.peers),
            State#msp_state{peers = NewPeers};
        false ->
            State
    end.

