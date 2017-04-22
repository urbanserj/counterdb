-module(counterdb_aae).
-behaviour(gen_server).

%% API
-export([
    start_link/0,
    start_link/1,

    snapshot/1,
    wait/1,

    inc_wait_aae_fun/2
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
    handle_info/2, terminate/2, code_change/3]).

-record(state, {
    id :: non_neg_integer(),
    tref :: reference()
}).

-define(PREFIX, "$AAE$-").
-define(WAITPREFIX, "$AAEWAIT$-").

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    start_link(server_id()).

start_link(ServerId) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [ServerId], []).

-spec snapshot(counterdb_storage:snapshot()) -> ok.
snapshot(Snapshot) ->
    Snapshot0 =
        maps:fold(
            fun (<<?PREFIX, _/binary>> =Key, Value, Acc) ->
                    Acc#{Key => Value};
                (<<?WAITPREFIX, _/binary>> =Key, Value, Acc) ->
                    Acc#{Key => Value};
                (Key, Value, Acc) ->
                    {ok, _, _} = counterdb:merge_value(Key, Value),
                    Acc
            end, #{}, Snapshot),
    % Process system keys only after public keys
    maps:fold(fun (Key, Value, ok) ->
        {ok, _, _} = counterdb:merge_value(Key, Value),
        case Key of
            <<?PREFIX, _/binary>> ->
                WaitAAEKey = wait_aaekey(Key),
                _ = counterdb_storage:update(WaitAAEKey, {?MODULE, inc_wait_aae_fun, []}),
                ok;
            _ -> ok
        end
   end, ok, Snapshot0).

-spec wait(timeout()) -> ok | {error, wait_timeout}.
wait(Timeout) ->
    % How does it work? See README.md
    TRef = erlang:send_after(Timeout, self(), wait_timeout),
    AAEKey = aaekey(),
    {ok, _, _} = counterdb:inc_value(AAEKey, 1),
    WaitAAEKey = wait_aaekey(AAEKey),
    Result = wait_loop(WaitAAEKey),
    _ = erlang:cancel_timer(TRef),
    Result.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([ServerId]) ->
    TRef = schedule_tick(),
    {ok, #state{id=ServerId, tref=TRef}}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({timeout, TRef, tick}, #state{tref=TRef}=State) ->
    lager:debug("Start AAE"),
    ok = active_anti_entropy(),
    lager:debug("Stop AAE"),
    {noreply, schedule_tick(State)};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec aaekey() -> binary().
aaekey() ->
    N = erlang:unique_integer([positive, monotonic]),
    NStr = integer_to_binary(N),
    ServerId = server_id(),
    SStr = integer_to_binary(ServerId),
    AAEKey = <<?PREFIX, SStr/binary, "-", NStr/binary>>,
    case counterdb:get_value(AAEKey) of
        {ok, 0, _} ->
            AAEKey;
        _ ->
            aaekey()
    end.

-spec wait_aaekey(binary()) -> binary().
wait_aaekey(<<?PREFIX, Key/binary>>) ->
    <<?WAITPREFIX, Key/binary>>.

-spec server_id() -> non_neg_integer().
server_id() ->
    {ok, ServerId} = application:get_env(counterdb, server_id),
    ServerId.

-spec schedule_tick(#state{}) -> #state{}.
schedule_tick(State) ->
    TRef = schedule_tick(),
    State#state{tref=TRef}.

-spec schedule_tick() -> reference().
schedule_tick() ->
    {ok, Timeout} = application:get_env(counterdb, aae_timeout),
    erlang:start_timer(Timeout, self(), tick).

-spec active_anti_entropy() -> ok.
active_anti_entropy() ->
    Nodes = counterdb_cluster:get(),
    active_anti_entropy(Nodes).

-spec active_anti_entropy([binary()] | undefined) -> ok.
active_anti_entropy(undefined) ->
    ok;
active_anti_entropy(Nodes) ->
    Snapshot = counterdb_storage:snapshot(),
    Data = term_to_binary(Snapshot),
    Size = length(Nodes),
    Timeout = 0, % one retry
    {_, _} = counterdb:send_and_wait(Size, {aae, Data}, Timeout),
    ok.

-spec inc_wait_aae_fun(ServerId, GCnt) -> GCnt
    when GCnt :: riak_dt_gcounter:gcounter(),
         ServerId :: non_neg_integer().
inc_wait_aae_fun(ServerId, not_found) ->
    riak_dt_gcounter:new(ServerId, 1);
inc_wait_aae_fun(ServerId, GCnt) ->
    % gcounter is opaque type :(
    % fixme; ordsets for wait aae key
    Bin = riak_dt:to_binary(GCnt),
    OrdDict = riak_dt:from_binary(Bin),
    case orddict:is_key(ServerId, OrdDict) of
        true -> GCnt;
        false ->
            {ok, GCnt0} = riak_dt_gcounter:update(increment, ServerId, GCnt),
            GCnt0
    end.

-spec wait_loop(binary()) -> ok | {error, wait_timeout}.
wait_loop(WaitAAEKey) ->
    WaitTimeout = 100,
    receive
        wait_timeout ->
            {error, wait_timeout}
    after WaitTimeout ->
        Size = length(counterdb_cluster:get()),
        case counterdb:get_value(WaitAAEKey) of
            {ok, Size, _Data} ->
                ok;
            {ok, _, _} ->
                wait_loop(WaitAAEKey)
        end
    end.
