-module(counterdb_storage).
-behaviour(gen_server).

%% API
-export([
    start_link/0,
    get/1,
    update/2,
    snapshot/0,
    data_dir/0
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
    handle_info/2, terminate/2, code_change/3]).

-export_type([name/0, value/0, snapshot/0, func/0]).

-record(state, {
    t :: tid(),
    id :: non_neg_integer()
}).

-type name() :: term().
-type value() :: term().
-type snapshot() :: #{binary() => term()}.
-type func() :: {module(), atom(), [any()]}.

-type tid() :: dets:tab_name().

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    start_link(server_id()).

start_link(ServerId) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [ServerId], []).

-spec get(name()) -> value() | not_found.
get(Name) ->
    gen_server:call(?MODULE, {get, Name}).

-spec update(name(), func()) -> value().
update(Name, MFA) ->
    gen_server:call(?MODULE, {update, Name, MFA}).

-spec snapshot() -> snapshot().
snapshot() ->
    gen_server:call(?MODULE, snapshot).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([ServerId]) ->
    {ok, T} = open_dets(ServerId),
    {ok, #state{t=T, id=ServerId}}.

handle_call({get, Name}, _From, #state{t=T}=State) ->
    Value = get(T, Name),
    {reply, Value, State};
handle_call({update, Name, MFA}, _From, #state{t=T, id=ServerId}=State) ->
    Value = update(ServerId, T, Name, MFA),
    {reply, Value, State};
handle_call(snapshot, _From, #state{t=T}=State) ->
    Value =
        dets:foldl(fun ({K, V}, Acc) ->
            Acc#{K => V}
        end, #{}, T),
    {reply, Value, State};
handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec get(tid(), name()) -> value() | not_found.
get(T, Name) ->
    case dets:lookup(T, Name) of
        [] -> not_found;
        [{Name, V}] -> V
    end.

-spec update(ServerId, tid(), name(), func()) -> value()
    when ServerId :: non_neg_integer().
update(ServerId, T, Name, {M, F, A}) ->
    Value = get(T, Name),
    Value0 = apply(M, F, [ServerId, Value|A]),
    ok = dets:insert(T, {Name, Value0}),
    ok = dets:sync(T),
    Value0.

-spec server_id() -> non_neg_integer().
server_id() ->
    {ok, ServerId} = application:get_env(counterdb, server_id),
    ServerId.

-spec data_dir() -> file:name().
data_dir() ->
    data_dir(server_id()).

-spec data_dir(ServerId :: non_neg_integer()) -> file:name().
data_dir(ServerId) ->
    DataDir = "data",
    case file:make_dir(DataDir) of
        ok -> ok;
        {error, eexist} -> ok
    end,
    ServerDataDir = filename:join(DataDir, integer_to_list(ServerId)),
    case file:make_dir(ServerDataDir) of
        ok -> ok;
        {error, eexist} -> ok
    end,
    ServerDataDir.

-spec open_dets(ServerId :: non_neg_integer()) ->
    {ok, tid()} | {error, term()}.
open_dets(ServerId) ->
    DataDir = data_dir(ServerId),
    Filename = filename:join(DataDir, "counters.dets"),
    dets:open_file(Filename, []).
