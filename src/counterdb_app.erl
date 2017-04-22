-module(counterdb_app).
-behaviour(application).
-export([
    start/2,
    start/0,
    stop/1,
    main/1
]).

start(_StartType, _StartArgs) ->
    ok = set_server_id(),
    counterdb_sup:start_link().

start() ->
    {ok, _} = application:ensure_all_started(counterdb),
    ok.

main([ServerIdStr]) ->
    ok = application:load(lager),
    ok = application:set_env(lager, handlers, [{lager_console_backend, info}]),
    ok = application:set_env(lager, crash_log, false),

    ok = application:load(counterdb),
    ok = application:set_env(counterdb, server_id, server_id(ServerIdStr)),

    {ok, _} = application:ensure_all_started(counterdb),
    lager:info("Ready!"),
    Ref = make_ref(),
    receive
        Ref -> ok
    end;
main(_Args) ->
    io:format(standard_error, "Usage: ./counterdb <id>~n", []),
    erlang:halt(1).

stop(_State) ->
    ok.

set_server_id() ->
    case os:getenv("COUNTERDB_ID") of
        Str when is_list(Str) ->
            ServerId = server_id(Str),
            ok = application:set_env(counterdb, server_id, ServerId);
        false -> ok
    end.

server_id(Value) when is_integer(Value), Value >= 0 ->
    Value;
server_id(Str) when is_list(Str) ->
    Value = list_to_integer(Str),
    server_id(Value).
