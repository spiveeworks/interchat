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
        Message ->
            io:format("Got unknown message: ~p~n", Message),
            loop(State)
    end.

accept(State = #ss{socket = Socket}, IP, Port) ->
    case gen_udp:send(Socket, IP, Port, "connection accepted") of
        ok ->
            io:format("~s:~w connected.~n", [inet:ntoa(IP), Port]),
            Connections = [{IP, Port} | State#ss.connections],
            State#ss{connections = Connections};
        {error, Reason} ->
            io:format("Connection reply failed with reason ~p.~n", Reason),
            State
    end.

