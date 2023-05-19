-module(interchat_server).

-export([start/0]).

-record(ss, {socket :: gen_udp:socket(), connections = []}).

start() ->
    io:format("Server started.~n", []),
    {ok, Socket} = gen_udp:open(7777, [{active, once}]),
    loop(#ss{socket = Socket}).

loop(State = #ss{socket = Socket}) ->
    receive
        {udp, Socket, IP, Port, "interchat connect"} ->
            NewState = accept(State, IP, Port),
            inet:setopts(Socket, [{active, once}]),
            loop(NewState);
        {udp, Socket, IP, Port, "username: " ++ Username} ->
            NewState = check_username_chosen(State, IP, Port, Username),
            inet:setopts(Socket, [{active, once}]),
            loop(NewState);
        Message ->
            io:format("Got unknown message: ~p~n", [Message]),
            loop(State)
    end.

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


