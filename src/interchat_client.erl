-module(interchat_client).

-export([start/0]).
-export([prompt_worker/1]).

-type input_mode() :: normal
                      | {username, inet:ip_address(), inet:port_number(), none | integer()}
                      | {send_username, inet:ip_address(), inet:port_number(),
                         integer(), string(), reference()}.

% TODO: Make connect a pending command? Make join an input_mode command?
-type pending_command() :: {join, string()}.

-record(server_connection, {address :: {inet:ip_address(), inet:port_number()},
                            username :: string(),
                            % This is not just a two-way map, different streams
                            % are used for outgoing vs incoming, but for
                            % incoming we want to know the channel from the
                            % stream, to interpret updates from the stream,
                            % whereas for outgoing we want to know the stream
                            % from the channel, to know where to send things
                            % to.
                            streams_out = #{} :: #{string() => integer()},
                            streams_in = #{} :: #{integer() => string()},
                            % A third set of streams are used for command
                            % negotiations.
                            pending_commands = #{} :: #{integer() => pending_command()}}).

-record(cs, {msp_proc :: pid(),
             servers = [] :: [#server_connection{}],
             input_mode = normal :: input_mode(),
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
    Server = #server_connection{address = {IP, Port},
                                  username = Username},
    Servers = [Server | State#cs.servers],
    NewState = State#cs{servers = Servers, input_mode = normal},
    prompt_and_loop(NewState);
process_datagram(State = #cs{input_mode = {send_username, IP, Port, StreamID, _Username, TRef}},
                 IP, Port, StreamID, _Index, true, "taken") ->
    erlang:cancel_timer(TRef),
    io:format("Username taken. Try again.~n", []),
    NewState = State#cs{input_mode = {username, IP, Port, StreamID}},
    prompt_and_loop(NewState);
process_datagram(State, IP, Port, StreamID, Index, Yield, Data) ->
    case lists:keyfind({IP, Port}, #server_connection.address, State#cs.servers) of
        Connection = #server_connection{} ->
            process_datagram2(State, IP, Port, StreamID, Index, Yield, Data, Connection);
        false ->
            io:format("\rUnknown address ~s:~p sent a datagram: ~s~n",
                      [inet:ntoa(IP), Port, Data]),
            loop(State)
    end.


process_datagram2(State, IP, Port, StreamID, 0, false, "messages in " ++ ChannelName, Connection) ->
    % Add the stream.
    Streams = maps:put(StreamID,
                       ChannelName,
                       Connection#server_connection.streams_in),
    % Now rebuild all the state.
    NewConnection = Connection#server_connection{streams_in = Streams},
    Connections = lists:keystore({IP, Port}, #server_connection.address,
                                 State#cs.servers, NewConnection),
    NewState = State#cs{servers = Connections},
    loop(NewState);
process_datagram2(State, IP, Port, _StreamID, 0, _Yield, Data, _Connection) ->
    % TODO: Reject?
    io:format("\r~s:~p started a new stream with unexpected payload: ~s~n",
                      [inet:ntoa(IP), Port, Data]),
    loop(State);
process_datagram2(State, IP, Port, StreamID, Index, Yield, Data, Connection) ->
    case maps:find(StreamID, Connection#server_connection.streams_in) of
        {ok, ChannelName} ->
            try_display_message(State, IP, Port, ChannelName, Data),
            % TODO: Print a new '>' prompt if we just overwrote one with \r?
            loop(State);
        error ->
            process_datagram3(State, IP, Port, StreamID, Index, Yield, Data,
                              Connection)
    end.

process_datagram3(State, IP, Port, StreamID, Index, Yield, Data, Connection) ->
    case maps:take(StreamID, Connection#server_connection.pending_commands) of
        {{join, Channel}, StillPending} ->
            NewConnection = Connection#server_connection{pending_commands = StillPending},
            Connections = lists:keystore({IP, Port}, #server_connection.address,
                                         State#cs.servers, NewConnection),
            NewState = State#cs{servers = Connections},
            case Data of
                "successfully joined" ->
                    io:format("\rJoined channel ~s.~n", [Channel]),
                    NewState2 = NewState#cs{current_channel = {IP, Port, Channel, none}},
                    % TODO: Print a new '>' prompt if we just overwrote one
                    % with \r?
                    loop(NewState2);
                "already joined" ->
                    io:format("\rAlready in channel ~s.~n", [Channel]),
                    % TODO: Print a new '>' prompt if we just overwrote one
                    % with \r?
                    loop(NewState)
            end;
        error ->
            % TODO: Reject the stream.
            % TODO: Move log_datagram from interchat_server to msp, and use it
            % here as well?
            YieldStr = case Yield of
                           true  -> "(yield)";
                           false -> "(...)"
                       end,
            io:format("\r~s:~w sent unexpected packet ~p.~p: ~s ~s~n",
                      [inet:ntoa(IP), Port, StreamID, Index, Data, YieldStr]),
            % TODO: Print a new '>' prompt if we just overwrote one with \r?
            loop(State)
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
    % TODO: Make this functionality generic? The server side does the exact
    % same stuff.
    Datagram = "messages in " ++ Channel,
    {ok, StreamID} = msp:open_stream(State#cs.msp_proc, IP, Port, self(), Datagram, false),
    {value, Connection, OtherConnections} = lists:keytake({IP, Port},
                                                          #server_connection.address,
                                                          State#cs.servers),
    Streams = maps:put(Channel, StreamID,
                       Connection#server_connection.streams_out),
    NewConnection = Connection#server_connection{streams_out = Streams},
    Connections = [NewConnection | OtherConnections],
    NewState = State#cs{servers = Connections,
                        current_channel = {IP, Port, Channel, StreamID}},
    send_message(NewState, IP, Port, Channel, StreamID, Str);
send_message(State, IP, Port, _Channel, StreamID, Str) ->
    % The address and stream already uniquely identify our username and the
    % channel.
    {ok, _} = msp:append_stream(State#cs.msp_proc, IP, Port, StreamID, Str, false),
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

join(State = #cs{servers = [Connection]}, Channel) ->
    {IP, Port} = Connection#server_connection.address,
    {ok, StreamID} = msp:open_stream(State#cs.msp_proc, IP, Port, self(), "join " ++ Channel, true),
    io:format("Attempting to join...~n", []),
    NewPendingCommands = maps:put(StreamID, {join, Channel},
                                  Connection#server_connection.pending_commands),
    NewConnection = Connection#server_connection{pending_commands =
                                                 NewPendingCommands},
    % We could call lists:keystore/4, but there's nothing else in the list, so
    % we can just build the singleton manually.
    State#cs{servers = [NewConnection]};
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

try_display_message(State, IP, Port, ChannelName, Data = "sender: " ++ Rest) ->
    case string:split(Rest, ", ts: ") of
        [Sender, Rest2] ->
            case string:to_integer(Rest2) of
                {TS, ", payload: " ++ Payload} ->
                    display_message(State, IP, Port, ChannelName, Sender, TS, Payload),
                    ok;
                {_, _} ->
                    message_invalid(IP, Port, ChannelName, Data)
            end;
        _ ->
            message_invalid(IP, Port, ChannelName, Data)
    end;
try_display_message(_State, IP, Port, ChannelName, Data) ->
    message_invalid(IP, Port, ChannelName, Data).

display_message(_State, _IP, _Port, ChannelName, Sender, TS, Payload) ->
    {_, {H, M, S}} = calendar:system_time_to_local_time(TS, millisecond),
    % include a carriage return, to overwrite the prompt symbol.
    io:format("\r[~s at ~p:~p:~p] ~s: ~s~n", [ChannelName, H, M, S, Sender, Payload]),
    ok.

message_invalid(_IP, _Port, ChannelName, Data) ->
    io:format("\rUnexpected message in stream for channel ~s: ~s~n",
              [ChannelName, Data]).

stop_fast() ->
    % Here we should make sure that the environment is clean enough to just
    % throw everything out. This is an unusual practice for servers, but this
    % isn't a server; we don't want to waste the user's time being too careful.
    erlang:halt().

