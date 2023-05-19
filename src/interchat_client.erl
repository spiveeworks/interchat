-module(interchat_client).

-export([start/0]).
-export([prompt_worker/1]).

-record(cs, {socket, servers = []}).

start() ->
    io:format("Interchat client started. Enter '/help' for a list of commands.~n", []),
    {ok, Socket} = gen_udp:open(0),
    spawn(?MODULE, prompt_worker, [self()]),
    loop(#cs{socket = Socket}).

prompt_worker(PID) ->
    Str = prompt(),
    PID ! {user_input, Str},
    ok.

loop(State) ->
    receive
        {user_input, Str} ->
            NewState = parse(State, Str),
            spawn(?MODULE, prompt_worker, [self()]),
            loop(NewState);
        Message ->
            io:format("Client loop got unknown message:~n~p~n", [Message]),
            loop(State)
    end.

parse(State, Str) ->
    case word(Str) of
        {"/help", Rest} ->
            help(Rest),
            State;
        {"/quit", ""} ->
            stop_fast();
        {"/quit", _} ->
            io:format("/quit does not take any arguments~n", []),
            State;
        {"/connect", ""} ->
            io:format("Please specify a server to connect to.~n", []),
            loop(State);
        {"/connect", A} ->
            NewState = connect(State, A),
            NewState;
        {[$/ | A] , _} ->
            io:format("Unknown command '/~s'.~n", [A]),
            State;
        {_, _} ->
            io:format("No conversation has been selected.~n", []),
            State
    end.

prompt() ->
    Input = io:get_line(">"),
    case string:trim(Input) of
        "" -> prompt();
        NonEmpty -> NonEmpty
    end.

word(Str) ->
    case string:split(Str, " ") of
        [SingleWord] ->
            {SingleWord, ""};
        [First, Rest] ->
            RestTrimmed = string:trim(Rest, leading),
            {First, RestTrimmed}
    end.

connect(State, A) ->
    case string:split(A, ":", trailing) of
        [Host] ->
            connect(State, Host, 7777);
        [H, PStr] ->
            case string:to_integer(PStr) of
                {P, ""} ->
                    connect(State, H, P);
                {_, _} ->
                    io:format("Error: ~s is not a valid port number.~n", [PStr]),
                    State
            end
    end.

connect(State = #cs{socket = Socket}, Host, Port) ->
    case gen_udp:send(Socket, Host, Port, "interchat connect") of
        ok ->
            {ok, IP} = inet:getaddr(Host, inet),
            await_connect(State, IP, Port);
        {error, Reason} ->
            io:format("Error: ~p~n", [Reason]),
            State
    end.

await_connect(State = #cs{socket = Socket}, IP, Port) ->
    receive
        {udp, Socket, IP, Port, "connection accepted"} ->
            io:format("Connection successful.~n", []),
            Servers = [{IP, Port} | State#cs.servers],
            State#cs{servers = Servers}
    after
        5000 ->
            io:format("Connection timed out.~n", []),
            State
    end.

help(_) ->
    io:format("/help - display this help message.~n", []),
    io:format("/quit - close the client.~n", []),
    ok.

stop_fast() ->
    % Here we should make sure that the environment is clean enough to just
    % throw everything out. This is an unusual practice for servers, but this
    % isn't a server; we don't want to waste the user's time being too careful.
    erlang:halt().

