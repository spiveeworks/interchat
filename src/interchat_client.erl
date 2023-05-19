-module(interchat_client).

-export([start/0]).
-export([prompt_worker/1]).

-type input_mode() :: normal
                      | {connect, inet:ip_address(), inet:port_number(), reference()}
                      | {login, inet:ip_address(), inet:port_number()}.

-record(cs, {socket, servers = [], input_mode = normal :: input_mode()}).

start() ->
    io:format("Interchat client started. Enter '/help' for a list of commands.~n", []),
    {ok, Socket} = gen_udp:open(0),
    prompt_and_loop(#cs{socket = Socket}).

prompt_worker(PID) ->
    Str = prompt(),
    PID ! {user_input, Str},
    ok.

prompt_and_loop(State) ->
    maybe_spawn_prompt(State),
    loop(State).

loop(State = #cs{socket = Socket}) ->
    receive
        {user_input, Str} ->
            NewState = dispatch_input(State, Str),
            prompt_and_loop(NewState);
        {udp, Socket, IP, Port, "connection accepted"} ->
            case State#cs.input_mode of
                {connect, IP, Port, TRef} ->
                    erlang:cancel_timer(TRef),
                    io:format("Server found. What is your username?~n", []),
                    NewState = State#cs{input_mode = {login, IP, Port}},
                    prompt_and_loop(NewState);
                _ ->
                    loop(State)
            end;
        {timeout, TRef, {connect, IP, Port}} ->
            case State#cs.input_mode of
                {connect, IP, Port, TRef} ->
                    io:format("Connection timed out.~n", []),
                    prompt_and_loop(State#cs{input_mode = normal});
                _ ->
                    loop(State)
            end;
        Message ->
            io:format("Client loop got unknown message:~n~p~n", [Message]),
            loop(State)
    end.

dispatch_input(State = #cs{input_mode = normal}, Str) ->
    parse(State, Str);
dispatch_input(State = #cs{input_mode = {login, IP, Port}}, Str) ->
    reply_login(State, IP, Port, Str);
dispatch_input(State = #cs{input_mode = Mode}, _) ->
    io:format("Got unexpected input from prompt worker?~n", []),
    io:format("Mode is ~p.~n", [Mode]),
    State.

maybe_spawn_prompt(#cs{input_mode = normal}) ->
    spawn(?MODULE, prompt_worker, [self()]);
maybe_spawn_prompt(#cs{input_mode = {login, _, _}}) ->
    spawn(?MODULE, prompt_worker, [self()]);
maybe_spawn_prompt(#cs{input_mode = _}) ->
    ok.

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

reply_login(State, IP, Port, Str) ->
    case word(Str) of
        {[$/ | _], ""} ->
            % FIXME: Usernames need to be a much more specific range of
            % characters than that!
            io:format("Usernames cannot begin with a slash.~n", []),
            State;
        {Username, ""} ->
            io:format("Hello ~s.~n", [Username]),
            Servers = [{IP, Port, Username} | State#cs.servers],
            State#cs{servers = Servers, input_mode = normal};
        {_, _} ->
            io:format("Usernames must be a single word.~n", []),
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
            TRef = erlang:start_timer(5000, self(), {connect, IP, Port}),
            State#cs{input_mode = {connect, IP, Port, TRef}};
        {error, Reason} ->
            io:format("Error: ~p~n", [Reason]),
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

