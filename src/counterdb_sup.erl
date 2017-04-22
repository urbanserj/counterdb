-module(counterdb_sup).
-behaviour(supervisor).
-export([start_link/0, init/1]).

-define(CHILD(Id), #{id => Id, start => {Id, start_link, []}}).

start_link() ->
    {ok, Port} = application:get_env(counterdb, port),
    Dispatch = cowboy_router:compile([
        {'_', [
            {<<"/config">>, counterdb_cluster_handler, []},
            {<<"/counter/:name/value">>, counterdb_handler, value},
            {<<"/counter/:name/consistent_value">>, counterdb_handler, consistent_value},
            {<<"/counter/:name">>, counterdb_handler, update},
            {<<"/counter/:name/merge">>, counterdb_handler, merge},
            {<<"/aae">>, counterdb_aae_handler, []}
        ]}
    ]),
    {ok, _} = cowboy:start_http(
        counterdb_listener, 64, [{port, Port}],
        [{env, [{dispatch, Dispatch}]}]
    ),
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {#{}, [
        ?CHILD(counterdb_cluster),
        ?CHILD(counterdb_storage),
        ?CHILD(counterdb_aae)
    ]}}.
