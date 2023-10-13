-module(interchat_client).

-export([start/0]).
-export([prompt_worker/1]).

-type input_mode() :: normal
                      | {connect, inet:ip_address(), inet:port_number(),
                         reference()}
                      | {username, inet:ip_address(), inet:port_number()}
                      | {send_username, inet:ip_address(), inet:port_number(),
                         string(), reference()}.


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

loop(State = #cs{socket = Socket, input_mode = InputMode}) ->
    receive
        {user_input, Str} ->
            NewState = dispatch_input(State, Str),
            prompt_and_loop(NewState);
        {udp, Socket, IP, Port, "connection accepted"} ->
            case InputMode of
                {connect, IP, Port, TRef} ->
                    erlang:cancel_timer(TRef),
                    io:format("Server found. What is your username?~n", []),
                    NewState = State#cs{input_mode = {username, IP, Port}},
                    prompt_and_loop(NewState);
                _ ->
                    loop(State)
            end;
        {udp, Socket, IP, Port, "already connected"} ->
            case InputMode of
                {connect, IP, Port, TRef} ->
                    erlang:cancel_timer(TRef),
                    io:format("The server says you are already connected to it.~n", []),
                    NewState = State#cs{input_mode = normal},
                    prompt_and_loop(NewState);
                _ ->
                    loop(State)
            end;
        {udp, Socket, IP, Port, "username accepted: " ++ Username} ->
            case InputMode of
                {send_username, IP, Port, Username, TRef} ->
                    erlang:cancel_timer(TRef),
                    io:format("Login successful.~n", []),
                    Servers = [{IP, Port, Username} | State#cs.servers],
                    NewState = State#cs{servers = Servers, input_mode = normal},
                    prompt_and_loop(NewState);
                _ ->
                    io:format("Late login success? Ignoring.~n", []),
                    loop(State)
            end;
        {udp, Socket, IP, Port, "username taken: " ++ Username} ->
            case InputMode of
                {send_username, IP, Port, Username, TRef} ->
                    erlang:cancel_timer(TRef),
                    io:format("Username taken. Try again.~n", []),
                    NewState = State#cs{input_mode = {username, IP, Port}},
                    prompt_and_loop(NewState);
                _ ->
                    loop(State)
            end;
        {udp, Socket, IP, Port, "message from " ++ Rest} ->
            display_message(State, IP, Port, Rest),
            loop(State);
        {timeout, TRef, {connect, IP, Port}} ->
            case InputMode of
                {connect, IP, Port, TRef} ->
                    io:format("Connection timed out.~n", []),
                    prompt_and_loop(State#cs{input_mode = normal});
                _ ->
                    loop(State)
            end;
        {timeout, TRef, {send_username, IP, Port, Username}} ->
            case InputMode of
                {send_username, IP, Port, Username, TRef} ->
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
dispatch_input(State = #cs{input_mode = {username, IP, Port}}, Str) ->
    reply_login(State, IP, Port, Str);
dispatch_input(State = #cs{input_mode = Mode}, _) ->
    io:format("Got unexpected input from prompt worker?~n", []),
    io:format("Mode is ~p.~n", [Mode]),
    State.

maybe_spawn_prompt(#cs{input_mode = normal}) ->
    spawn(?MODULE, prompt_worker, [self()]);
maybe_spawn_prompt(#cs{input_mode = {username, _, _}}) ->
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
            State;
        {"/connect", A} ->
            NewState = connect(State, A),
            NewState;
        {"/join", ""} ->
            io:format("Please specify a channel to join.~n", []),
            State;
        {"/join", A} ->
            NewState = join(State, A),
            NewState;
        {[$/ | A] , _} ->
            io:format("Unknown command '/~s'.~n", [A]),
            State;
        {_, _} ->
            io:format("No conversation has been selected.~n", []),
            State
    end.

reply_login(State = #cs{socket = Socket}, IP, Port, Str) ->
    case word(Str) of
        {[$/ | _], ""} ->
            % FIXME: Usernames need to be a much more specific range of
            % characters than that!
            io:format("Usernames cannot begin with a slash.~n", []),
            State;
        {Username, ""} ->
            case gen_udp:send(Socket, IP, Port, "username: " ++ Username) of
                ok ->
                    TRef = erlang:start_timer(5000, self(), {send_username, IP, Port, Username}),
                    State#cs{input_mode = {send_username, IP, Port, Username, TRef}};
                {error, Reason} ->
                    io:format("Error: ~p~n", [Reason]),
                    State
            end;
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

join(State = #cs{servers = [{IP, Port, _}]}, Channel) ->
    case gen_udp:send(State#cs.socket, IP, Port, "join " ++ Channel) of
        ok ->
            io:format("Attempting to join...~n", []),
            State;
        {error, Reason} ->
            io:format("Error: ~p~n", [Reason]),
            State
    end;
join(State = #cs{servers = []}, _) ->
    io:format("Error: Use /connect to connect to a server first.~n"),
    State;
join(State, _) ->
    io:format("Error: Multi-login not yet implemented.~n"),
    State.


help(_) ->
    io:format("/help - display this help message.~n", []),
    io:format("/quit - close the client.~n", []),
    ok.

display_message(State, IP, Port, Rest) ->
    case string:split(Rest, ", ts: ") of
        [Sender, Rest2] ->
            case string:to_integer(Rest2) of
                {TS, ", payload: " ++ Payload} ->
                    display_message2(State, IP, Port, Sender, TS, Payload);
                {_, _} ->
                    error
            end;
        _ ->
            error
    end.

display_message2(_State, _IP, _Port, Sender, TS, Payload) ->
    {_, {H, M, S}} = calendar:system_time_to_local_time(TS, millisecond),
    io:format("\r[~p:~p:~p] ~s: ~s~n", [H, M, S, Sender, Payload]),
    ok.

stop_fast() ->
    % Here we should make sure that the environment is clean enough to just
    % throw everything out. This is an unusual practice for servers, but this
    % isn't a server; we don't want to waste the user's time being too careful.
    erlang:halt().

