-module(interchat_server).

-export([start/0]).

-record(ss, {socket :: gen_udp:socket()}).

start() ->
    io:format("Server started.~n", []),
    {ok, Socket} = gen_udp:open(7777, [{active, once}]),
    loop(#ss{socket = Socket}).

loop(State = #ss{socket = Socket}) ->
    receive
        {udp, Socket, IP, InPortNo, Packet} ->
            io:format("Got datagram: {~p, ~p, ~p}~n", [IP, InPortNo, Packet]),
            inet:setopts(Socket, [{active, once}]),
            loop(State);
        Message ->
            io:format("Got unknown message: ~p~n", Message),
            loop(State)
    end.

