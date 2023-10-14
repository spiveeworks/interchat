-module(interchat_server).

-export([start/0, state/0]).

-record(message, {sender :: string() | server,
                  remaining_users :: [string()],
                  timestamp_ms :: integer(), % milliseconds since epoch
                  message :: string()}).

-record(channel, {users = [] :: [string()], mailbox = []}).

-record(ss, {msp_proc :: pid(),
             connections = [],
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
        {udp, Socket, IP, Port, "interchat connect"} ->
            %NewState = accept(State, IP, Port),
            NewState = State,
            %inet:setopts(Socket, [{active, once}]),
            loop(NewState);
        {udp, Socket, IP, Port, "username: " ++ Username} ->
            %NewState = check_username_chosen(State, IP, Port, Username),
            NewState = State,
            %inet:setopts(Socket, [{active, once}]),
            loop(NewState);
        {udp, Socket, IP, Port, "join " ++ ChannelName} ->
            NewState = join_channel(State, IP, Port, ChannelName),
            inet:setopts(Socket, [{active, once}]),
            loop(NewState);
        {udp, Socket, IP, Port, "message in " ++ Rest} ->
            NewState = add_message(State, IP, Port, Rest),
            inet:setopts(Socket, [{active, once}]),
            loop(NewState);
        {udp, Socket, IP, Port, Datagram} ->
            io:format("~s:~w sent unexpected message ~s.~n", [inet:ntoa(IP), Port, Datagram]),
            inet:setopts(Socket, [{active, once}]),
            loop(State);
        {msp_connect, IP, Port} ->
            % Log, but don't do anything until we get a username, since the msp
            % process is already tracking the connection itself for us.
            io:format("~s:~w connected.~n", [inet:ntoa(IP), Port]),
            loop(State);
        Message ->
            io:format("Got unknown message: ~p~n", [Message]),
            loop(State)
    end.

-ifdef(comment_out).
accept(State, IP, Port) ->
    Connections = State#ss.connections,
    Socket = State#ss.socket,
    case lists:keymember({IP, Port}, 1, Connections) of
        true ->
            case gen_udp:send(Socket, IP, Port, "already connected") of
                ok ->
                    io:format("~s:~w tried to connect again.~n", [inet:ntoa(IP), Port]),
                    State;
                {error, Reason} ->
                    io:format("Connection reply failed with reason ~p.~n", [Reason]),
                    State
            end;
        false ->
            case gen_udp:send(Socket, IP, Port, "connection accepted") of
                ok ->
                    io:format("~s:~w connected.~n", [inet:ntoa(IP), Port]),
                    NewConnections = [{{IP, Port}, not_chosen} | State#ss.connections],
                    State#ss{connections = NewConnections};
                {error, Reason} ->
                    io:format("Connection reply failed with reason ~p.~n", [Reason]),
                    State
            end
    end.

check_username_chosen(State, IP, Port, Username) ->
    Connections = State#ss.connections,
    Socket = State#ss.socket,
    case lists:keyfind({IP, Port}, 1, Connections) of
        {{IP, Port}, not_chosen} ->
            case lists:keymember(Username, 2, Connections) of
                true ->
                    case gen_udp:send(Socket, IP, Port, "username taken: " ++ Username) of
                        ok ->
                            State;
                        {error, Reason} ->
                            io:format("Connection reply failed with reason ~p.~n", [Reason]),
                            State
                    end;
                false ->
                    case gen_udp:send(Socket, IP, Port, "username accepted: " ++ Username) of
                        ok ->
                            io:format("~s:~w chose username ~s.~n", [inet:ntoa(IP), Port, Username]),
                            NewConnections = lists:keystore({IP, Port}, 1, Connections, {{IP, Port}, Username}),
                            State#ss{connections = NewConnections};
                        {error, Reason} ->
                            io:format("Connection reply failed with reason ~p.~n", [Reason]),
                            State
                    end
            end;
        {{IP, Port}, _} ->
            % Maybe this packet was sent earlier and arrived late. Ignore.
            State;
        false ->
            State
    end.
-endif.

join_channel(State, IP, Port, ChannelName) ->
    % TODO: reply with error messages? Or assume they are malicious and ignore?
    case lists:keyfind({IP, Port}, 1, State#ss.connections) of
        {{IP, Port}, not_chosen} ->
            State;
        {{IP, Port}, Username} ->
            join_channel2(State, IP, Port, Username, ChannelName);
        false ->
            State
    end.

join_channel2(State, IP, Port, Username, ChannelName) ->
    Channel = maps:get(ChannelName, State#ss.channels, #channel{}),
    case lists:member(Username, Channel#channel.users) of
        true ->
            case gen_udp:send(State#ss.msp_proc, IP, Port, "already in channel: " ++ ChannelName) of
                ok ->
                    ok;
                {error, Reason} ->
                    io:format("Redundant join reply failed with reason ~p.~n", [Reason])
            end,
            State;
        false ->
            io:format("User ~s joined channel ~s.~n", [Username, ChannelName]),

            Users = [Username | Channel#channel.users],

            case gen_udp:send(State#ss.msp_proc, IP, Port, "joined channel: " ++ ChannelName) of
                ok ->
                    ok;
                {error, Reason} ->
                    io:format("Join reply failed with reason ~p.~n", [Reason])
            end,

            NewMessage = #message{sender = server,
                                  remaining_users = Channel#channel.users,
                                  timestamp_ms = os:system_time(millisecond),
                                  message = Username ++ " joined."},
            Messages = Channel#channel.mailbox ++ [NewMessage],
            NewChannelState = Channel#channel{users = Users, mailbox = Messages},

            NewChannels = maps:put(ChannelName, NewChannelState, State#ss.channels),
            NewState = State#ss{channels = NewChannels},

            update_channel(NewState, ChannelName)
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
    case lists:keyfind({IP, Port}, 1, State#ss.connections) of
        {{IP, Port}, not_chosen} ->
            State;
        {{IP, Port}, Username} ->
            add_message4(State, IP, Port, Username, ChannelName, Message, Channel);
        false ->
            State
    end.

add_message4(State, IP, Port, Username, ChannelName, Message, Channel) ->
    case lists:member(Username, Channel#channel.users) of
        true ->
            case gen_udp:send(State#ss.msp_proc, IP, Port, "message received") of
                ok ->
                    ok;
                {error, Reason} ->
                    io:format("Message reply failed with reason ~p.~n", [Reason])
            end,

            NewMessage = #message{sender = Username,
                                  remaining_users = lists:delete(Username, Channel#channel.users),
                                  timestamp_ms = os:system_time(millisecond),
                                  message = Message},
            Messages = Channel#channel.mailbox ++ [NewMessage],
            NewChannelState = Channel#channel{mailbox = Messages},

            NewChannels = maps:put(ChannelName, NewChannelState, State#ss.channels),
            NewState = State#ss{channels = NewChannels},

            update_channel(NewState, ChannelName);
        false ->
            State
    end.


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

try_send_message(Socket, Connections, UsersTried, Message, [Username | Rest], Acc) ->
    case lists:member(Username, UsersTried) of
        false ->
            case try_send_message_each(Socket, Connections, Message, Username) of
                ok ->
                    % success; remove from mailbox, TODO: do we really still mark user as tried?
                    try_send_message(Socket, Connections, [Username | UsersTried], Message, Rest,
                                     Acc);
                error ->
                    % failed; mark as tried, and also as not sent.
                    try_send_message(Socket, Connections, [Username | UsersTried],
                                     Message, Rest, [Username | Acc])
            end;
        true ->
            try_send_message(Socket, Connections, UsersTried, Message, Rest,
                             [Username | Acc])
    end;
try_send_message(_, _, UsersTried, _, [], Acc) ->
    NewRemaining = lists:reverse(Acc),
    {UsersTried, NewRemaining}.

try_send_message_each(Socket, Connections, Message, Username) ->
    case lists:keyfind(Username, 2, Connections) of
        {{IP, Port}, Username} ->
            try_send_message_each2(Socket, Message, IP, Port);
        false ->
            error
    end.

try_send_message_each2(Socket, Message, IP, Port) ->
    Datagram = message_datagram(Message),
    case gen_udp:send(Socket, IP, Port, Datagram) of
        ok ->
            ok;
        {error, _} ->
            error
    end.

message_datagram(#message{sender = Sender, timestamp_ms = TSMS, message = Payload}) ->
    SenderName = case Sender of
                     server -> "server";
                     _      -> Sender
                 end,
    IOL = io_lib:format("message from ~s, ts: ~p, payload: ~s", [SenderName, TSMS, Payload]),
    unicode:characters_to_list(IOL).

