-module(interchat_client).

-export([start/0]).

start() ->
    io:format("Hello world!~n", []),
    stop_fast().

stop_fast() ->
    % Here we should make sure that the environment is clean enough to just
    % throw everything out. This is an unusual practice for servers, but this
    % isn't a server; we don't want to waste the user's time being too careful.
    erlang:halt().

