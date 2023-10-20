-module(msp).

-behaviour(gen_server).

-export([start_link/1]).
-export([state/1, start_listening/3, connect/4, open_stream/6, append_stream/6,
         accept_packet/5, reject_packet/5, trust_stream/4]).
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

accept_packet(PID, IP, Port, StreamID, PacketID) ->
    gen_server:cast(PID, {accept_packet, IP, Port, StreamID, PacketID}).

reject_packet(PID, IP, Port, StreamID, PacketID) ->
    gen_server:cast(PID, {reject_packet, IP, Port, StreamID, PacketID}).

trust_stream(PID, IP, Port, StreamID) ->
    gen_server:cast(PID, {trust_stream, IP, Port, StreamID}).


-type endpoint() :: {inet:ip_address(), inet:port_number()}.

-record(msp_datagram, {index :: integer(),
                       direction :: in | out,
                       mode :: yield | hold | close,
                       % TODO: use binaries? IO lists?
                       payload :: string()}).
-record(msp_stream, {data = [] :: [#msp_datagram{}],
                     % How many packets have we/they acknowledged?
                     next_ack_index :: integer(),
                     % How many packets have been verified by userspace?
                     next_unverified_index :: integer(),
                     % How many packets have we received overall?
                     next_index :: integer(),
                     % If the stream didn't really start at datagram 0, how do
                     % we work out the user-facing stream index?
                     offset = 0 :: integer(),
                     has_control :: boolean(),
                     is_open :: boolean(),
                     event_listener :: pid(),
                     trusted = false :: boolean()}).
-record(msp_peer, {streams = #{} :: #{integer() => #msp_stream{}},
                   stream_open_listener :: pid(),
                   id_pattern :: even | odd}).
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

% Typically we need to dig down to the stream in question, modify it, and then
% build back to our overall state. TODO: Split up this process? Starting to see
% that deeply nested state is a heuristic in Erlang, that more concurrency is
% available for the taking... Is it always worth taking? I have little idea.
rebuild_state(State, IP, Port, Peer, StreamID, Stream) ->
    NewStreams = maps:put(StreamID, Stream, Peer#msp_peer.streams),
    NewPeer = Peer#msp_peer{streams = NewStreams},
    NewPeers = maps:put({IP, Port}, NewPeer, State#msp_state.peers),
    State#msp_state{peers = NewPeers}.

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
    {noreply, NewState};
handle_cast({accept_packet, IP, Port, StreamID, PacketID}, State) ->
    NewState = do_accept_packet(State, IP, Port, StreamID, PacketID),
    {noreply, NewState};
handle_cast({reject_packet, IP, Port, StreamID, PacketID}, State) ->
    NewState = do_reject_packet(State, IP, Port, StreamID, PacketID),
    {noreply, NewState};
handle_cast({trust_stream, IP, Port, StreamID}, State) ->
    NewState = do_trust_stream(State, IP, Port, StreamID),
    {noreply, NewState}.

handle_info({udp, Socket, IP, Port, Data}, State = #msp_state{socket = Socket}) ->
    NewState = do_receive(State, IP, Port, Data),
    inet:setopts(Socket, [{active, once}]),
    {noreply, NewState};
handle_info({timeout, TRef, connect}, State) ->
    NewState = do_connect_timeout(State, TRef),
    {noreply, NewState}.

-define(NO_YIELD, 0).
-define(YIELD, 1).
-define(CLOSE, 2).
-define(REQUEST_CLOSE, 3).
-define(ACKNOWLEDGE, 4).
-define(REJECT, 5).

do_receive(State = #msp_state{listeners = none}, IP, Port, "msp connect") ->
    io:format("~s:~w tried to connect. Discarding.~n", [inet:ntoa(IP), Port]),
    State;
do_receive(State, IP, Port, "msp connect") ->
    do_accept(State, IP, Port);
do_receive(State, IP, Port, "connection accepted") ->
    do_connection_success(State, IP, Port);
do_receive(State, IP, Port, [?NO_YIELD | Data]) ->
    do_receive2(State, IP, Port, hold, Data);
do_receive(State, IP, Port, [?YIELD | Data]) ->
    do_receive2(State, IP, Port, yield, Data);
do_receive(State, IP, Port, [?CLOSE | Data]) ->
    do_receive2(State, IP, Port, close, Data);
do_receive(State, IP, Port, [?ACKNOWLEDGE | Data]) ->
    do_receive_ack(State, IP, Port, Data);
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

do_receive_ack(State, IP, Port, Data) ->
    case decode_int(Data) of
        {ok, StreamID, Data2} ->
            do_receive_ack2(State, IP, Port, StreamID, Data2);
        error ->
            State
    end.

do_receive_ack2(State, IP, Port, StreamID, Data) ->
    case decode_int(Data) of
        {ok, AckIndex, Data2} ->
            do_receive_ack3(State, IP, Port, StreamID, AckIndex, Data2);
        error ->
            State
    end.

do_receive_ack3(State, IP, Port, StreamID, AckIndex, Data) ->
    case decode_int(Data) of
        {ok, NackIndex, ""} ->
            do_handle_ack(State, IP, Port, StreamID, AckIndex, NackIndex);
        _ ->
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
            Peer = #msp_peer{stream_open_listener = OpenListener,
                             % We are acting as a server, we get the nice even
                             % streams
                             id_pattern = even},
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
            % TODO: Should servers default to being trusted? or?
            Peer = #msp_peer{stream_open_listener = OpenListener,
                             % We are acting as a client, we get sad old odd
                             % streams
                             id_pattern = odd},
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
            {StreamID, Stream} = reopen_stream(Peer, HandlerPID),
            gen_server:reply(Caller, {ok, StreamID}),

            % Skip all of the guards of `append_stream`, since we already
            % passed or constructed the cases ourselves, just send the datagram
            % and rebuild.
            build_send_datagram(State, IP, Port, StreamID, FirstPayload,
                                FirstYield, Peer, Stream);
        error ->
            gen_server:reply(Caller, {error, not_connected})
    end.

reopen_stream(#msp_peer{streams = Streams, id_pattern = even}, HandlerPID) ->
    reopen_stream(Streams, HandlerPID, 0);
reopen_stream(#msp_peer{streams = Streams, id_pattern = odd}, HandlerPID) ->
    reopen_stream(Streams, HandlerPID, 1).

reopen_stream(Streams, HandlerPID, Attempt) ->
    case try_open_stream(Streams, HandlerPID, Attempt) of
        taken ->
            reopen_stream(Streams, HandlerPID, Attempt + 2);
        {ok, Stream} ->
            {Attempt, Stream}
    end.

try_open_stream(Streams, HandlerPID, ID) ->
    case maps:find(ID, Streams) of
        {ok, Stream = #msp_stream{is_open = false}} ->
            % Make sure to keep .data, since the close message might not have
            % delivered yet, if it came from us.
            % offset should already be set at the point where we sent/received
            % the datagram.
            NewStream = Stream#msp_stream{offset = -Stream#msp_stream.next_index,
                                          has_control = true,
                                          is_open = true,
                                          event_listener = HandlerPID},
            {ok, NewStream};
        {ok, _} ->
            taken;
        error ->
            Stream = #msp_stream{next_ack_index = 0,
                                 next_unverified_index = 0,
                                 next_index = 0,
                                 has_control = true,
                                 is_open = true,
                                 event_listener = HandlerPID},
            {ok, Stream}
    end.

is_stream_opened_by_us(#msp_peer{id_pattern = even}, StreamID) ->
    StreamID rem 2 == 0;
is_stream_opened_by_us(#msp_peer{id_pattern = odd}, StreamID) ->
    StreamID rem 2 /= 0.

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
do_append_stream3(State, _, _, _, _, _, Caller, _, #msp_stream{is_open = false}) ->
    gen_server:reply(Caller, {error, stream_closed}),
    State;
do_append_stream3(State, IP, Port, StreamID, Payload, Yield, Caller, Peer, Stream) ->
    Index = Stream#msp_stream.next_index + Stream#msp_stream.offset,
    gen_server:reply(Caller, {ok, Index}),

    build_send_datagram(State, IP, Port, StreamID, Payload, Yield, Peer, Stream).

build_send_datagram(State, IP, Port, StreamID, Payload, Yield, Peer, Stream) ->
    Index = Stream#msp_stream.next_index,
    Datagram = #msp_datagram{index = Index,
                             direction = out,
                             mode = Yield,
                             payload = Payload},
    send_datagram(State, IP, Port, StreamID, Datagram),

    % Add the tracking info for this new stream.
    Data = [Datagram | Stream#msp_stream.data],
    HasControl = case Yield of
                     yield -> false;
                     hold -> true;
                     close -> is_stream_opened_by_us(Peer, StreamID)
                 end,
    Offset = case Yield of
                 close -> -(Index + 1);
                 _     -> Stream#msp_stream.offset
             end,
    NewStream = Stream#msp_stream{data = Data,
                                  next_index = Index + 1,
                                  % Also treat previous packets as verified and
                                  % acknowledged, since a response implicitly
                                  % acknowledges the thing it is responding to,
                                  % and things we are sending do not need us to
                                  % acknowledge them.
                                  next_ack_index = Index + 1,
                                  next_unverified_index = Index + 1,
                                  % Set the offset if we just closed the stream
                                  offset = Offset,
                                  has_control = HasControl,
                                  is_open = Yield /= closed},
    rebuild_state(State, IP, Port, Peer, StreamID, NewStream).

send_datagram(State, IP, Port, StreamID, DatagramInfo) ->
    YieldByte = case DatagramInfo#msp_datagram.mode of
                    yield  -> ?YIELD;
                    hold -> ?NO_YIELD;
                    close -> ?CLOSE
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

do_handle_datagram2(State, IP, Port, Peer, StreamID, Index, Yield, Payload) ->
    case maps:find(StreamID, Peer#msp_peer.streams) of
        {ok, Stream} ->
            do_handle_datagram3(State, IP, Port, Peer, StreamID, Stream, Index,
                                Yield, Payload);
        error ->
            do_handle_new_datagram(State, IP, Port, Peer, StreamID, Index,
                                   Yield, Payload)
    end.

do_handle_datagram3(State, IP, Port, Peer, StreamID, Stream, Index, Yield, Payload) ->
    Next = Stream#msp_stream.next_index,
    case Stream#msp_stream.has_control of
        false when Index >= Next ->
            case lists:keymember(Index, #msp_datagram.index, Stream#msp_stream.data) of
                false ->
                    % the fact that we got a reply means anything we sent was also
                    % acknowledged, so we should treat those packets like they were
                    % acknowledged, which means removing them.
                    NewStream = remove_all_outgoing_packets(Stream),

                    Datagram = #msp_datagram{index = Index,
                                             direction = in,
                                             mode = Yield,
                                             payload = Payload},
                    Data = [Datagram | NewStream#msp_stream.data],
                    NewStream2 = NewStream#msp_stream{data = Data},
                    NewStream3 = send_incoming_packets(IP, Port, Peer, StreamID, NewStream2),

                    % If the stream is trusted, then we might be able to ACK
                    % right now.
                    NewStream4 = send_ack_if_any(State#msp_state.socket, IP,
                                                 Port, StreamID, NewStream3),

                    rebuild_state(State, IP, Port, Peer, StreamID, NewStream4);
                true ->
                    % TODO: Reject the stream.
                    State
            end;
        _ when Index < Next ->
            % TODO: Acknowledge the packet again?
            State;
        _ ->
            % TODO: Reject the stream?
            State
    end.

-ifdef(comment).
f() ->
            % TODO: Notify user that things were implicitly acknowledged?
            ok.
-endif.

do_handle_new_datagram(State, IP, Port, Peer, StreamID, Index, Yield, Payload) ->
    % Check whether they should even be opening this stream.
    case is_stream_opened_by_us(Peer, StreamID) of
        false ->
            StreamListener = Peer#msp_peer.stream_open_listener,
            Datagram = #msp_datagram{index = Index,
                                     direction = in,
                                     mode = Yield,
                                     payload = Payload},
            % TODO: Separate the opening notification from the actual flushing,
            % until an external process explicitly registers to the stream?
            Stream = #msp_stream{data = [Datagram],
                                 next_ack_index = 0,
                                 next_unverified_index = 0,
                                 next_index = 0,
                                 has_control = false,
                                 is_open = true,
                                 event_listener = StreamListener},
            Stream2 = send_incoming_packets(IP, Port, Peer, StreamID, Stream),

            rebuild_state(State, IP, Port, Peer, StreamID, Stream2);
        true ->
            % They sent us a datagram on a channel we are responsible for
            % opening. TODO: Reject the stream.
            State
    end.

send_incoming_packets(_IP, _Port, _Peer, _StreamID, Stream = #msp_stream{has_control = true}) ->
    % TODO: check that the data buffer is completely empty? since we only just
    % told the user about some packets, and the peer just yielded, so they
    % shouldn't have sent more packets after.
    Stream;
send_incoming_packets(IP, Port, Peer, StreamID, Stream) ->
    Index = Stream#msp_stream.next_index,
    case lists:keytake(Index, #msp_datagram.index, Stream#msp_stream.data) of
        {value, Datagram, Data} ->
            Yield = Datagram#msp_datagram.mode,
            Payload = Datagram#msp_datagram.payload,
            AppIndex = Index + Stream#msp_stream.offset,
            erlang:send(Stream#msp_stream.event_listener,
                        {msp_datagram, IP, Port, StreamID, AppIndex, Yield, Payload}),
            NextUnverified = case Stream#msp_stream.trusted of
                                 true  -> Index + 1;
                                 false -> Stream#msp_stream.next_unverified_index
                             end,
            HasControl = case Yield of
                             yield -> true;
                             hold -> false;
                             close -> is_stream_opened_by_us(Peer, StreamID)
                         end,
            NewStream = Stream#msp_stream{data = Data,
                                          next_index = Index + 1,
                                          next_unverified_index = NextUnverified,
                                          has_control = HasControl,
                                          is_open = Yield /= close},
            send_incoming_packets(IP, Port, Peer, StreamID, NewStream);
        false ->
            Stream
    end.

do_accept_packet(State, IP, Port, StreamID, PacketID) ->
    % Trust the process casting to us, for now, and just peel off the layers.
    Peer = maps:get({IP, Port}, State#msp_state.peers),
    Stream = maps:get(StreamID, Peer#msp_peer.streams),

    case PacketID >= Stream#msp_stream.next_unverified_index of
        true ->
            % Mark more packets as verified, and potentially acknowledge them.
            NewStream = Stream#msp_stream{next_unverified_index = PacketID + 1},

            NewStream2 = check_ack(State, IP, Port, StreamID, NewStream),
            rebuild_state(State, IP, Port, Peer, StreamID, NewStream2);
        false ->
            % Already verified, do nothing.
            State
    end.

check_ack(State, IP, Port, StreamID, Stream) ->
    case Stream#msp_stream.next_unverified_index < Stream#msp_stream.next_index of
        % We have stuff we have already received, that user code might still
        % accept/reject, wait for that before we give feedback... Unless they
        % are sending so much stuff, that we should acknowledge what we have so
        % far, just so they know they are streaming successfully. TODO: wait
        % until our mailbox is empty, to see if more things were verified? Or,
        % if not empty, until the next mailbox tic?
        % TODO: If the user doesn't get around to verifying some packets,
        % should we acknowledge what they *have* verified? Time out and reject
        % by default? Dunno...
        true  -> send_ack_if_backlogged(State#msp_state.socket, IP, Port,
                                        StreamID, Stream);

        % Either we are waiting on more from the peer, or we are waiting on a
        % reply from the user. If we had a reply prepared, we would have
        % verified the received packets by a different path, and already sent
        % or planned to send the reply AS the ACK. Since there is no reply
        % prepared, and no more packets left for the user to verify, we should
        % ACK what we have gotten, if anything. TODO: wait until our mailbox is
        % empty, to see if there was more from the peer, and/or a reply from
        % the user, and/or more from the peer?
        false -> send_ack_if_any(State#msp_state.socket, IP, Port, StreamID,
                                   Stream)
    end.

send_ack_if_any(Socket, IP, Port, StreamID, Stream) ->
    Ack = Stream#msp_stream.next_ack_index - 1,
    Verified = Stream#msp_stream.next_unverified_index - 1,
    case Verified > Ack of
        true ->
            Verified = Stream#msp_stream.next_unverified_index - 1,
            send_ack(Socket, IP, Port, StreamID, Verified),
            Stream#msp_stream{next_ack_index = Verified + 1};
        false ->
            Stream
    end.

send_ack_if_backlogged(Socket, IP, Port, StreamID, Stream) ->
    Ack = Stream#msp_stream.next_ack_index - 1,
    Verified = Stream#msp_stream.next_unverified_index - 1,
    case Verified > Ack + 10 of
        true ->
            Verified = Stream#msp_stream.next_unverified_index - 1,
            % TODO: Are there security considerations in regards to exposing
            % our ability to keep up with the stream? Should we just verify
            % multiples of 10 or whatever?
            send_ack(Socket, IP, Port, StreamID, Verified),
            Stream#msp_stream{next_ack_index = Verified + 1};
        false ->
            Stream
    end.

send_ack(Socket, IP, Port, StreamID, AckIndex) ->
    Data = [?ACKNOWLEDGE,
            encode_int(StreamID),
            encode_int(AckIndex),
            % If this is greater than AckIndex + 1, then it indicates a NACK
            % for all the packets in between. TODO: Actually calculate how much
            % to NACK.
            encode_int(0)],
    ok = gen_udp:send(Socket, IP, Port, Data),
    ok.

do_reject_packet(State, _IP, _Port, _StreamID, _PacketID) ->
    io:format("Warning: Packet rejection is not yet implemented.~n", []),
    State.

do_trust_stream(State, IP, Port, StreamID) ->
    Peer = maps:get({IP, Port}, State#msp_state.peers),
    Stream = maps:get(StreamID, Peer#msp_peer.streams),

    % Mark the stream as trusted, and mark any packets we have already received
    % as verified.
    NewStream = Stream#msp_stream{next_unverified_index = Stream#msp_stream.next_index,
                                  trusted = true},
    % Acknowledge anything that we have already been sent, now that it is all
    % trusted.
    NewStream2 = send_ack_if_any(State#msp_state.socket, IP, Port, StreamID,
                                 NewStream),

    rebuild_state(State, IP, Port, Peer, StreamID, NewStream2).

do_handle_ack(State, IP, Port, StreamID, AckIndex, NackIndex) ->
    case maps:find({IP, Port}, State#msp_state.peers) of
        {ok, Peer} ->
            do_handle_ack2(State, IP, Port, Peer, StreamID, AckIndex,
                           NackIndex);
        error ->
            State
    end.

do_handle_ack2(State, IP, Port, Peer, StreamID, AckIndex, NackIndex) ->
    case maps:find(StreamID, Peer#msp_peer.streams) of
        {ok, Stream} ->
            do_handle_ack3(State, IP, Port, Peer, StreamID, Stream, AckIndex,
                           NackIndex);
        error ->
            State
    end.

do_handle_ack3(State, IP, Port, Peer, StreamID, Stream, AckIndex, _NackIndex) ->
    NewStream = remove_acknowledged_packets(Stream, AckIndex),

    % TODO: Notify stream event listener that the packets were accepted.

    rebuild_state(State, IP, Port, Peer, StreamID, NewStream).

remove_all_outgoing_packets(Stream) ->
    F = fun(#msp_datagram{direction = Direction}) ->
                Direction /= out
        end,
    Data = lists:filter(F, Stream#msp_stream.data),

    Stream#msp_stream{data = Data}.

remove_acknowledged_packets(Stream, AckIndex) ->
    F = fun(#msp_datagram{index = Index}) ->
                Index > AckIndex
        end,
    Data = lists:filter(F, Stream#msp_stream.data),

    Stream#msp_stream{data = Data}.

