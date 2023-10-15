-module(interchat_client).

-export([start/0]).
-export([prompt_worker/1]).

-type input_mode() :: normal
                      | {username, inet:ip_address(), inet:port_number(), none | integer()}
                      | {send_username, inet:ip_address(), inet:port_number(),
                         integer(), string(), reference()}.

% TODO: Make connect a pending command? Make join an input_mode command?
-type pending_command() :: {join, string()}.

-record(cs, {msp_proc :: pid(),
             servers = [],
             input_mode = normal :: input_mode(),
             pending_commands = #{} :: #{integer() => pending_command()},
             % TODO: Make this tuple an actual record?
             current_channel = none :: none | {inet:ip_address(), inet:port_number(), string(), none | integer()}}).

start() ->
    io:format("Interchat client started. Enter '/help' for a list of commands.~n", []),
    {ok, MSP} = msp:start_link(0),
    prompt_and_loop(#cs{msp_proc = MSP}).

prompt_worker(PID) ->
    Str = prompt(),
    PID ! {user_input, Str},
    ok.

prompt_and_loop(State) ->
    maybe_spawn_prompt(State),
    loop(State).

loop(State = #cs{input_mode = InputMode}) ->
    receive
        {user_input, eof} ->
            % Print a newline for unix shells/files.
            io:format("~n", []),
            stop_fast();
        {user_input, Str} ->
            NewState = dispatch_input(State, Str),
            prompt_and_loop(NewState);
        {msp_datagram, IP, Port, StreamID, Index, Yield, Data} ->
            process_datagram(State, IP, Port, StreamID, Index, Yield, Data);
        {udp, _Socket, IP, Port, "message from " ++ Rest} ->
            display_message(State, IP, Port, Rest),
            loop(State);
        % TODO: Make MSP handle this timeout and give an msp_timout message, so
        % we don't have to cancel the timer ourselves on success.
        {timeout, TRef, {send_username, IP, Port, Username}} ->
            case InputMode of
                {send_username, IP, Port, _StreamID, Username, TRef} ->
                    io:format("Connection timed out.~n", []),
                    prompt_and_loop(State#cs{input_mode = normal});
                _ ->
                    loop(State)
            end;
        Message ->
            io:format("\rClient loop got unknown message:~n~p~n", [Message]),
            loop(State)
    end.

process_datagram(State = #cs{input_mode = {send_username, IP, Port, StreamID, Username, TRef}},
                 IP, Port, StreamID, _Index, true, "accepted") ->
    % TODO: What if they reply but with the wrong yield field/etc? We could
    % detect incorrect replies early and stop waiting immediately.
    erlang:cancel_timer(TRef),
    io:format("Login successful.~n", []),
    Servers = [{IP, Port, Username} | State#cs.servers],
    NewState = State#cs{servers = Servers, input_mode = normal},
    prompt_and_loop(NewState);
process_datagram(State = #cs{input_mode = {send_username, IP, Port, StreamID, _Username, TRef}},
                 IP, Port, StreamID, _Index, true, "taken") ->
    erlang:cancel_timer(TRef),
    io:format("Username taken. Try again.~n", []),
    NewState = State#cs{input_mode = {username, IP, Port, StreamID}},
    prompt_and_loop(NewState);
process_datagram(State, IP, Port, StreamID, _Index, _Yield, Data) ->
    case maps:take(StreamID, State#cs.pending_commands) of
        {{join, Channel}, StillPending} ->
            NewState = State#cs{pending_commands = StillPending},
            case Data of
                "successfully joined" ->
                    io:format("Joined channel ~s.~n", [Channel]),
                    NewState2 = NewState#cs{current_channel = {IP, Port, Channel, none}},
                    prompt_and_loop(NewState2);
                "already joined" ->
                    io:format("Already in channel ~s.~n", [Channel]),
                    prompt_and_loop(NewState)
            end;
        error ->
            % TODO: Use the more detailed message format that the server uses?
            % TODO: Actually reject the message?
            io:format("\rClient loop got unexpected datagram: ~s~n", [Data]),
            prompt_and_loop(State)
    end.

dispatch_input(State = #cs{input_mode = normal}, Str) ->
    parse(State, Str);
dispatch_input(State = #cs{input_mode = {username, IP, Port, StreamID}}, Str) ->
    reply_login(State, IP, Port, Str, StreamID);
dispatch_input(State = #cs{input_mode = Mode}, _) ->
    io:format("Got unexpected input from prompt worker?~n", []),
    io:format("Mode is ~p.~n", [Mode]),
    State.

maybe_spawn_prompt(#cs{input_mode = normal}) ->
    spawn(?MODULE, prompt_worker, [self()]);
maybe_spawn_prompt(#cs{input_mode = {username, _, _, _}}) ->
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
        _ ->
            % Not a command, just send it as a chat message.
            case State#cs.current_channel of
                none ->
                    io:format("No channel has been selected.~n", []),
                    State;
                {IP, Port, Channel, StreamID} ->
                    send_message(State, IP, Port, Channel, StreamID, Str)
            end
    end.

send_message(State, IP, Port, Channel, none, Str) ->
    Datagram = "message in " ++ Channel ++ ": " ++ Str,
    {ok, StreamID} = msp:open_stream(State#cs.msp_proc, IP, Port, self(), Datagram, false),
    State#cs{current_channel = {IP, Port, Channel, StreamID}};
send_message(State, IP, Port, Channel, StreamID, Str) ->
    Datagram = "message in " ++ Channel ++ ": " ++ Str,
    {ok, _} = msp:append_stream(State#cs.msp_proc, IP, Port, StreamID, Datagram, false),
    State.

reply_login(State, IP, Port, Str, StreamID) ->
    case word(Str) of
        {[$/ | _], ""} ->
            % FIXME: Usernames need to be a much more specific range of
            % characters than that!
            io:format("Usernames cannot begin with a slash.~n", []),
            State;
        {Username, ""} ->
            reply_login2(State, IP, Port, Username, StreamID);
        {_, _} ->
            io:format("Usernames must be a single word.~n", []),
            State
    end.

reply_login2(State, IP, Port, Username, none) ->
    {ok, StreamID} = msp:open_stream(State#cs.msp_proc, IP, Port, self(), "username: " ++ Username, true),
    reply_login3(State, IP, Port, Username, StreamID);
reply_login2(State, IP, Port, Username, StreamID) ->
    {ok, _} = msp:append_stream(State#cs.msp_proc, IP, Port, StreamID, "username: " ++ Username, true),
    reply_login3(State, IP, Port, Username, StreamID).

reply_login3(State, IP, Port, Username, StreamID) ->
    TRef = erlang:start_timer(5000, self(), {send_username, IP, Port, Username}),
    State#cs{input_mode = {send_username, IP, Port, StreamID, Username, TRef}}.


prompt() ->
    case io:get_line(">") of
        eof ->
            eof;
        Input ->
            case string:trim(Input) of
                "" ->
                    prompt();
                NonEmpty ->
                    NonEmpty
            end
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

connect(State, Host, Port) ->
    {ok, IP} = inet:getaddr(Host, inet),
    io:format("Connecting to ~s...~n", [Host]),
    case msp:connect(State#cs.msp_proc, IP, Port, self()) of
        ok ->
            io:format("Server found. What is your username?~n", []),
            State#cs{input_mode = {username, IP, Port, none}};
        {error, timeout} ->
            io:format("Connection timed out.~n", []),
            State
    end.

join(State = #cs{servers = [{IP, Port, _}]}, Channel) ->
    {ok, StreamID} = msp:open_stream(State#cs.msp_proc, IP, Port, self(), "join " ++ Channel, true),
    io:format("Attempting to join...~n", []),
    NewPendingCommands = maps:put(StreamID, {join, Channel},
                                  State#cs.pending_commands),
    State#cs{pending_commands = NewPendingCommands};
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
    % include a carriage return, to overwrite the prompt symbol.
    io:format("\r[~p:~p:~p] ~s: ~s~n", [H, M, S, Sender, Payload]),
    ok.

stop_fast() ->
    % Here we should make sure that the environment is clean enough to just
    % throw everything out. This is an unusual practice for servers, but this
    % isn't a server; we don't want to waste the user's time being too careful.
    erlang:halt().

