-module(counterdb_cluster).
-behaviour(gen_server).

%% API
-export([
    start_link/0,
    get/0,
    set/1
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
    handle_info/2, terminate/2, code_change/3]).

-record(state, {
    list :: [binary()] | undefined
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec get() -> [binary()] | undefined.
get() ->
    gen_server:call(?MODULE, get).

-spec set([binary()] | undefined) -> ok | {error, already}.
set(List) ->
    gen_server:call(?MODULE, {set, List}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    List =
        case read_cluster_info() of
            {ok, Term} when is_list(Term) ->
                Term;
            {error, enoent} ->
                undefined
        end,
    {ok, #state{list=List}}.

handle_call(get, _From, #state{list=List}=State) ->
    {reply, List, State};
handle_call({set, List}, _From, #state{list=undefined}=State) ->
    lager:info("Init cluster: ~p~n", [List]),
    ok = save_cluster_info(List),
    {reply, ok, State#state{list=List}};
handle_call({set, _}, _From, State) ->
    {reply, {error, already}, State};
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

-spec read_cluster_info() -> {ok, term()} | {error, file:posix()}.
read_cluster_info() ->
    Filename = cluster_dat(),
    case file:read_file(Filename) of
        {ok, Data} ->
            {ok, binary_to_term(Data)};
        {error, Error} ->
            {error, Error}
    end.

-spec save_cluster_info(term()) -> ok.
save_cluster_info(Term) ->
    Filename = cluster_dat(),
    ok = file:write_file(Filename, term_to_binary(Term)).

-spec cluster_dat() -> file:name().
cluster_dat() ->
    DataDir = counterdb_storage:data_dir(),
    filename:join(DataDir, "cluster.dat").
