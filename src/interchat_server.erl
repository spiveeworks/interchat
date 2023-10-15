-module(interchat_server).

-export([start/0, state/0]).

-record(message, {sender :: string() | server,
                  remaining_users :: [string()],
                  timestamp_ms :: integer(), % milliseconds since epoch
                  message :: string()}).

-record(channel, {users = [] :: [string()], mailbox = []}).

-record(connection, {address :: {inet:ip_address(), inet:port_number()},
                     username = {not_chosen, no_stream} :: string() | {not_chosen, no_stream | integer()},
                     streams_up = #{} :: #{string() => integer()},
                     streams_down = #{} :: #{string() => integer()}}).

-record(ss, {msp_proc :: pid(),
             connections = [] :: [#connection{}],
             channels = #{} :: #{string() => #channel{}},
             lobby = 0 :: integer()}).

start() ->
    register(?MODULE, self()),
    {ok, MSP} = msp:start_link(7777),
    msp:start_listening(MSP, self(), self()),
    io:format("Server started.~n", []),
    loop(#ss{msp_proc = MSP}).

state() ->
    ?MODULE ! {get_state, self()},
    receive
        {interchat_server_state, State} ->
            State
    end.

loop(State) ->
    receive
        {get_state, PID} ->
            PID ! {interchat_server_state, State},
            loop(State);
        {udp, Socket, IP, Port, "message in " ++ Rest} ->
            NewState = add_message(State, IP, Port, Rest),
            inet:setopts(Socket, [{active, once}]),
            loop(NewState);
        {msp_connect, IP, Port} ->
            NewState = accept(State, IP, Port),
            loop(NewState);
        {msp_datagram, IP, Port, StreamID, 0, Yield, Data} ->
            NewState = handle_new_stream(State, IP, Port, StreamID, Yield, Data),
            loop(NewState);
        {msp_datagram, IP, Port, StreamID, Index, Yield, Data} ->
            NewState = handle_new_packet(State, IP, Port, StreamID, Index, Yield, Data),
            loop(NewState);
        Message ->
            io:format("Got unknown message: ~p~n", [Message]),
            loop(State)
    end.

accept(State, IP, Port) ->
    io:format("~s:~w connected.~n", [inet:ntoa(IP), Port]),
    NewConnections = [#connection{address = {IP, Port}} | State#ss.connections],
    State#ss{connections = NewConnections}.

handle_new_stream(State, IP, Port, StreamID, true, "username: " ++ Username) ->
    case lists:keyfind({IP, Port}, #connection.address, State#ss.connections) of
        Connection = #connection{username = {not_chosen, no_stream}} ->
            NewConnection = Connection#connection{username = {not_chosen, StreamID}},
            NewConnections = lists:keystore({IP, Port}, #connection.address,
                                            State#ss.connections, NewConnection),
            NewState = State#ss{connections = NewConnections},
            check_username_chosen(NewState, IP, Port, StreamID, Username);
        _ ->
            % Either they already started a stream for username negotiation, or
            % they already chose a username! TODO: Reject the stream.
            State
    end;
handle_new_stream(State, IP, Port, StreamID, true, "join " ++ ChannelName) ->
    join_channel(State, IP, Port, StreamID, ChannelName);
%handle_new_stream(State, IP, Port, StreamID, false, "post in " ++ ChannelName) ->
    %add_message_upload_stream(State, IP, Port, StreamID, ChannelName);
handle_new_stream(State, IP, Port, StreamID, Yield, Datagram) ->
    % TODO: Reject the stream.
    log_datagram(IP, Port, StreamID, 0, Yield, Datagram),
    State.

handle_new_packet(State, IP, Port, StreamID, _, true, "username: " ++ Username) ->
    case lists:keyfind({IP, Port}, #connection.address, State#ss.connections) of
        #connection{username = {not_chosen, StreamID}} ->
            % MSP says this is the next packet, and it is the right stream, so
            % we don't need to check anything else, other than the username
            % itself.
            check_username_chosen(State, IP, Port, StreamID, Username);
        _ ->
            % Either a stream has already been started, or they already chose a
            % username! TODO: Reject the stream.
            State
    end;
handle_new_packet(State, IP, Port, StreamID, PacketID, Yield, Datagram) ->
    % TODO: Reject the stream.
    log_datagram(IP, Port, StreamID, PacketID, Yield, Datagram),
    State.

log_datagram(IP, Port, StreamID, PacketID, Yield, Datagram) ->
    % TODO: Reject the stream.
    YieldStr = case Yield of
                   true  -> "(yield)";
                   false -> "(...)"
               end,
    io:format("~s:~w sent unexpected packet ~p.~p: ~s ~s~n",
              [inet:ntoa(IP), Port, StreamID, PacketID, Datagram, YieldStr]).


check_username_chosen(State, IP, Port, StreamID, Username) ->
    Connections = State#ss.connections,
    case lists:keymember(Username, #connection.username, Connections) of
        true ->
            {ok, _} = msp:append_stream(State#ss.msp_proc, IP, Port, StreamID, "taken", true),
            State;
        false ->
            % TODO: close stream?
            io:format("~s:~w chose username ~s.~n", [inet:ntoa(IP), Port, Username]),
            {ok, _} = msp:append_stream(State#ss.msp_proc, IP, Port, StreamID, "accepted", true),
            Connection = #connection{address = {IP, Port},
                                     username = Username},
            NewConnections = lists:keystore({IP, Port}, #connection.address,
                                            Connections, Connection),
            State#ss{connections = NewConnections}
    end.

join_channel(State, IP, Port, StreamID, ChannelName) ->
    % TODO: reply with error messages? Or assume they are malicious and ignore?
    case lists:keyfind({IP, Port}, #connection.address, State#ss.connections) of
        #connection{username = {not_chosen, _}} ->
            % TODO: Reject the packet
            State;
        #connection{username = Username} ->
            join_channel2(State, IP, Port, StreamID, Username, ChannelName);
        false ->
            State
    end.

join_channel2(State, IP, Port, StreamID, Username, ChannelName) ->
    Channel = maps:get(ChannelName, State#ss.channels, #channel{}),
    case lists:member(Username, Channel#channel.users) of
        true ->
            %TODO: Add a third yield mode, to close the stream.
            {ok, _} = msp:append_stream(State#ss.msp_proc, IP, Port, StreamID, "already joined", true),
            State;
        false ->
            io:format("User ~s joined channel ~s.~n", [Username, ChannelName]),

            Users = [Username | Channel#channel.users],
            NewChannel = Channel#channel{users = Users},

            {ok, _} = msp:append_stream(State#ss.msp_proc, IP, Port, StreamID, "successfully joined", true),

            NewMessage = #message{sender = server,
                                  remaining_users = Channel#channel.users,
                                  timestamp_ms = os:system_time(millisecond),
                                  message = Username ++ " joined."},

            {NewConnections, NewChannel2} = mailbox_add_message(State#ss.msp_proc,
                                                                State#ss.connections,
                                                                NewMessage,
                                                                ChannelName,
                                                                NewChannel),

            NewChannels = maps:put(ChannelName, NewChannel2, State#ss.channels),
            State#ss{connections = NewConnections, channels = NewChannels}
    end.

add_message(State, IP, Port, Rest) ->
    case string:split(Rest, ": ") of
        [_, ""] ->
            State;
        ["", _] ->
            State;
        [ChannelName, Message] ->
            add_message2(State, IP, Port, ChannelName, Message);
        _ ->
            State
    end.

add_message2(State, IP, Port, ChannelName, Message) ->
    case maps:find(ChannelName, State#ss.channels) of
        {ok, Channel} ->
            add_message3(State, IP, Port, ChannelName, Message, Channel);
        error ->
            State
    end.

add_message3(State, IP, Port, ChannelName, Message, Channel) ->
    % TODO: reply with error messages? Or assume they are malicious and ignore?
    case lists:keyfind({IP, Port}, #connection.address, State#ss.connections) of
        #connection{username = {not_chosen, _}} ->
            State;
        #connection{username = Username} ->
            add_message4(State, Username, ChannelName, Message, Channel);
        false ->
            State
    end.

add_message4(State, Username, ChannelName, Message, Channel) ->
    case lists:member(Username, Channel#channel.users) of
        true ->
            NewMessage = #message{sender = Username,
                                  remaining_users = lists:delete(Username, Channel#channel.users),
                                  timestamp_ms = os:system_time(millisecond),
                                  message = Message},
            {NewConnections, NewChannel} = mailbox_add_message(State#ss.msp_proc,
                                                               State#ss.connections,
                                                               NewMessage,
                                                               ChannelName,
                                                               Channel),

            NewChannels = maps:put(ChannelName, NewChannel, State#ss.channels),
            State#ss{connections = NewConnections, channels = NewChannels};
        false ->
            State
    end.

mailbox_add_message(MSP, Connections, Message, ChannelName, Channel) ->
    Datagram = message_datagram(Message),
    Users = Message#message.remaining_users,
    NewConnections = try_send_message(MSP, Connections, Datagram, ChannelName, Users),

    Messages = Channel#channel.mailbox ++ [Message],
    NewChannelState = Channel#channel{mailbox = Messages},
    {NewConnections, NewChannelState}.


try_send_message(MSP, Connections, Datagram, ChannelName, [Username | Rest]) ->
    case try_send_message_each(MSP, Connections, Datagram, ChannelName, Username) of
        {ok, NewConnections} ->
            try_send_message(MSP, NewConnections, Datagram, ChannelName, Rest);
        error ->
            try_send_message(MSP, Connections, Datagram, ChannelName, Rest)
    end;
try_send_message(_, ConnectionsAfter, _, _, []) ->
    ConnectionsAfter.

try_send_message_each(MSP, Connections, Datagram, ChannelName, Username) ->
    case lists:keytake(Username, #connection.username, Connections) of
        % TODO: If we don't end up modifying the connection, do we even need to
        % remove it and add it back? Would it be any cheaper not to keytake?
        {value, Connection, ConnectionsWithout} ->
            NewConnection = try_send_message_each_connection(MSP, Datagram, ChannelName, Connection),

            NewConnections = [NewConnection | ConnectionsWithout],
            {ok, NewConnections};
        false ->
            error
    end.

try_send_message_each_connection(MSP, Datagram, ChannelName, Connection) ->
    {IP, Port} = Connection#connection.address,
    case maps:find(ChannelName, Connection#connection.streams_down) of
        {ok, StreamID} ->
            {ok, _} = msp:append_stream(MSP, IP, Port, StreamID, Datagram,
                                        false),
            Connection;
        error ->
            {ok, StreamID} = msp:open_stream(MSP, IP, Port, self(),
                                             "messages in " ++ ChannelName,
                                             false),
            {ok, _} = msp:append_stream(MSP, IP, Port, StreamID, Datagram,
                                        false),
            NewStreams = maps:put(ChannelName, StreamID,
                                  Connection#connection.streams_down),
            Connection#connection{streams_down = NewStreams}
    end.

message_datagram(#message{sender = Sender, timestamp_ms = TSMS, message = Payload}) ->
    SenderName = case Sender of
                     server -> "server";
                     _      -> Sender
                 end,
    IOL = io_lib:format("sender: ~s, ts: ~p, payload: ~s", [SenderName, TSMS, Payload]),
    unicode:characters_to_list(IOL).

-ifdef(comment).
% We want to use this code as a basis for updating new logins with their
% mailbox
update_channel(State, ChannelName) ->
    case maps:find(ChannelName, State#ss.channels) of
        {ok, Channel} ->
            update_channel2(State, ChannelName, Channel);
        error ->
            State
    end.

update_channel2(State, ChannelName, Channel) ->
    Mailbox = Channel#channel.mailbox,
    NewMailbox = update_mailbox(State#ss.msp_proc, State#ss.connections, Mailbox),
    io:format("Mailbox updated.~n"),

    NewChannelState = Channel#channel{mailbox = NewMailbox},
    NewChannels = maps:put(ChannelName, NewChannelState, State#ss.channels),
    State#ss{channels = NewChannels}.

update_mailbox(Socket, Connections, Mailbox) ->
    update_mailbox(Socket, Connections, Mailbox, [], []).

update_mailbox(Socket, Connections, [Message | Rest], UsersTried, Acc) ->
    {NewUsers, NewMessage} = update_message(Socket, Connections, UsersTried, Message),
    NewAcc = case NewMessage of
                 remove -> Acc;
                 _      -> [NewMessage | Acc]
             end,
    update_mailbox(Socket, Connections, Rest, NewUsers, NewAcc);
update_mailbox(_, _, [], _, Acc) ->
    lists:reverse(Acc).

update_message(Socket, Connections, UsersTried, Message) ->
    RemainingUsers = Message#message.remaining_users,
    {NewTried, NewRemaining} = try_send_message(Socket, Connections, UsersTried,
                                                Message, RemainingUsers, []),
    NewMessage = case NewRemaining of
                     [] -> remove;
                     _  -> Message#message{remaining_users = NewRemaining}
                 end,
    {NewTried, NewMessage}.
-endif.

