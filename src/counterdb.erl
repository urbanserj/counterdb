-module(counterdb).

-export([
    main/1,

    inc_value/2,
    inc_value/3,
    inc_gcnt_fun/3,

    merge_value/2,
    merge_gcnt_fun/3,

    get_value/1,
    get_consistent_value/1,
    get_consistent_value/2,

    send_and_wait/2,
    send_and_wait/3
]).

-export_type([result/0, gcnt/0, wait/0, request/0]).

main(Args) ->
    counterdb_app:main(Args).

-type gcnt() :: riak_dt_gcounter:gcounter().
-type result() :: {ok | wait_timeout, Value :: non_neg_integer(), ValueRaw :: binary()}.
-type wait() :: pos_integer() | undefined.

-spec inc_value(counterdb_storage:name(), non_neg_integer()) -> result().
inc_value(Name, 0) ->
    get_value(Name);
inc_value(Name, Inc) ->
    Value = counterdb_storage:update(Name, {?MODULE, inc_gcnt_fun, [Inc]}),
    to_result(ok, Value).

-spec inc_value(wait(), counterdb_storage:name(), non_neg_integer()) ->
    result() | {error, term()}.
inc_value(undefined, Name, Inc) ->
    inc_value(Name, Inc);
inc_value(Wait, Name, Inc) ->
    {ok, _, ValueBin} = inc_value(Name, Inc),
    {Type, List} = send_and_wait(Wait, {merge, Name, ValueBin}),
    Value = gcounter_merge([ValueBin|List]),
    to_result(Type, Value).

-spec merge_value(counterdb_storage:name(), binary() | gcnt()) -> result().
merge_value(Name, Value) when is_binary(Value) ->
    merge_value(Name, riak_dt:from_binary(Value));
merge_value(Name, GCnt) ->
    Value = counterdb_storage:update(Name, {?MODULE, merge_gcnt_fun, [GCnt]}),
    to_result(ok, Value).

-spec get_value(counterdb_storage:name()) -> result().
get_value(Name) ->
    Value = counterdb_storage:get(Name),
    to_result(ok, Value).

-spec get_consistent_value(counterdb_storage:name()) -> result().
get_consistent_value(Name) ->
    {ok, Timeout} = application:get_env(counterdb, max_wait_timeout),
    case counterdb_aae:wait(Timeout) of
        ok -> get_value(Name);
        {error, wait_timeout} ->
            {ok, Value, ValueBin} = get_value(Name),
            {wait_timeout, Value, ValueBin}
    end.

-spec get_consistent_value(wait(), counterdb_storage:name()) ->
    result().
get_consistent_value(undefined, Name) ->
    get_consistent_value(Name);
get_consistent_value(Wait, Name) ->
    {Type, List} = send_and_wait(Wait, {get_value, Name}),
    Value = gcounter_merge(List),
    to_result(Type, Value).

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec inc_gcnt_fun(ServerId, GCnt | not_found, Inc) -> GCnt
    when ServerId :: non_neg_integer(), GCnt :: gcnt(),
         Inc :: non_neg_integer().
inc_gcnt_fun(ServerId, not_found, Inc) ->
    riak_dt_gcounter:new(ServerId, Inc);
inc_gcnt_fun(ServerId, GCnt, Inc) ->
    {ok, GCnt0} = riak_dt_gcounter:update({increment, Inc}, ServerId, GCnt),
    GCnt0.

-spec merge_gcnt_fun(ServerId, GCnt | not_found, GCnt) -> GCnt
    when ServerId :: non_neg_integer(), GCnt :: gcnt().
merge_gcnt_fun(_ServerId, not_found, GCnt) ->
    GCnt;
merge_gcnt_fun(_ServerId, GCntA, GCntB) ->
    riak_dt_gcounter:merge(GCntA, GCntB).

-spec to_result(Type :: atom(), gcnt() | not_found) -> result().
to_result(Atom, not_found) ->
    Value = riak_dt_gcounter:new(),
    to_result(Atom, Value);
to_result(Atom, Value) ->
    {Atom, riak_dt_gcounter:value(Value), riak_dt:to_binary(Value)}.

-spec gcounter_merge([gcnt() | binary()]) -> gcnt().
gcounter_merge([]) ->
    riak_dt_gcounter:new();
gcounter_merge([Bin]) when is_binary(Bin) ->
    riak_dt:from_binary(Bin);
gcounter_merge([Head|Tail]) ->
    lists:foldl(fun (Value, GCntB) ->
        GCntA = gcounter_merge([Value]),
        riak_dt_gcounter:merge(GCntA, GCntB)
    end, gcounter_merge([Head]), Tail).

%%%===================================================================
%%% Send and wait
%%%===================================================================

-type request() :: {get_value, counterdb_storage:name()}
                 | {merge, counterdb_storage:name(), binary()}
                 | {inc, counterdb_storage:name(), non_neg_integer()}
                 | {aae, binary()}.

-spec send_and_wait(wait(), request()) ->
    {ok | wait_timeout, [binary()]}.
send_and_wait(Wait, Request) ->
    {ok, Timeout} = application:get_env(counterdb, max_wait_timeout),
    send_and_wait(Wait, Request, Timeout).

-spec send_and_wait(wait(), request(), timeout()) ->
    {ok | wait_timeout, [binary()]}.
send_and_wait(Wait, Request, Timeout) ->
    TRef = erlang:send_after(Timeout, self(), wait_timeout),
    Nodes = counterdb_cluster:get(),
    Result = send_and_wait(Nodes, Wait, Request, []),
    _ = erlang:cancel_timer(TRef),
    Result.

-spec send_and_wait([binary()], counterdb_storage:wait(), request(), [binary()]) ->
    {ok | wait_timeout, [binary()]}.
send_and_wait(_Nodes, Wait, _Request, Acc) when Wait =< 0 ->
    {ok, Acc};
send_and_wait([], _Wait, _Request, Acc) ->
    {ok, Acc};
send_and_wait(Nodes, Wait, Request, Acc) ->
    Map = send_async_requests(Nodes, Request),
    Result = wait_async_responses(Map, Wait),
    {Nodes0, Acc0, Wait0} =
        maps:fold(
            fun (_Node, {status, 204}, {N, R, W}) ->
                    {N, R, W - 1};
                (Node, {_ErrorOrStatus, _}, {N, R, W}) ->
                    {[Node|N], R, W};
                (_Node, Data, {N, R, W}) ->
                    {N, [Data|R], W - 1}
            end, {[], Acc, Wait}, Result),
    {ok, WaitTimeout} = application:get_env(counterdb, retry_wait_timeout),
    receive
        wait_timeout ->
            {wait_timeout, Acc0}
    after WaitTimeout ->
        send_and_wait(Nodes0, Wait0, Request, Acc0)
    end.


-spec send_async_requests([binary()], request()) -> #{reference() => binary()}.
send_async_requests(Nodes, Request) ->
    lists:foldl(fun (Node, Acc) ->
        {ok, Ref} =
            case send_async_request(Node, Request) of
                {ok, R} -> {ok, R};
                {error, Error} ->
                    R = make_ref(),
                    self() ! {hackney_response, R, {error, Error}},
                    {ok, R}
            end,
        Acc#{Ref => Node}
    end, #{}, Nodes).

-spec wait_async_responses(#{reference() => binary()}, wait()) ->
    #{binary() => binary() | {error, term()} | {status, pos_integer()}}.
wait_async_responses(Map, Wait) ->
    wait_loop(Map, Wait, #{}).

-spec wait_loop(#{reference() => binary()}, wait(), Res) -> Res
    when Res :: #{binary() => binary()
                            | {error, term()}
                            | {status, pos_integer()}}.
wait_loop(Map, _Wait, Acc) when map_size(Map) =:= 0 ->
    Acc;
wait_loop(Map, Wait, Acc) ->
    receive
        {hackney_response, Ref, done} ->
            case maps:take(Ref, Map) of
                error ->
                    wait_loop(Map, Wait, Acc);
                {_Node, Map0} ->
                    wait_loop(Map0, Wait - 1, Acc)
            end;
        {hackney_response, Ref, Data} when is_binary(Data) ->
            case maps:get(Ref, Map, undefined) of
                undefined ->
                    wait_loop(Map, Wait, Acc);
                Node ->
                    wait_loop(Map, Wait, Acc#{Node => Data})
            end;
        {hackney_response, Ref, {error, Error}} ->
            case maps:take(Ref, Map) of
                error ->
                    wait_loop(Map, Wait, Acc);
                {Node, Map0} ->
                    wait_loop(Map0, Wait - 1, Acc#{Node => {error, Error}})
            end;
        {hackney_response, Ref, {status, Code, _CodeStr}} ->
            case maps:get(Ref, Map, undefined) of
                undefined ->
                    wait_loop(Map, Wait, Acc);
                Node ->
                    wait_loop(Map, Wait, Acc#{Node => {status, Code}})
            end;
        {hackney_response, _Ref, _Skip} ->
            wait_loop(Map, Wait, Acc)
    end.

%%%===================================================================
%%% Send request
%%%===================================================================

-spec send_async_request(binary(), request()) ->
    {ok, reference()} | {error, term()}.
send_async_request(Node, Request) ->
    HackneyArgs =
        [F(Node, Request) || F <- [
            fun method/2,
            fun url/2,
            fun headers/2,
            fun body/2,
            fun option/2
        ]],
    apply(hackney, request, HackneyArgs).

method(_Node, {get_value, _Name}) ->
    'get';
method(_Node, {merge, _Name, _ValueRaw}) ->
    'post';
method(_Node, {inc, _Name, _Inc}) ->
    'post';
method(_Node, {aae, _Data}) ->
    'post'.

url(Node, {get_value, Name}) ->
    <<"http://", Node/binary, ":", (port_bin())/binary,
        "/counter/", Name/binary, "/value">>;
url(Node, {merge, Name, _ValueRaw}) ->
    <<"http://", Node/binary, ":", (port_bin())/binary,
        "/counter/", Name/binary, "/merge">>;
url(Node, {inc, Name, _Inc}) ->
    <<"http://", Node/binary, ":", (port_bin())/binary,
        "/counter/", Name/binary>>;
url(Node, {aae, _Data}) ->
    <<"http://", Node/binary, ":", (port_bin())/binary, "/aae">>.

headers(_Node, {get_value, _Name}) ->
    [{<<"Accept">>, <<"application/x-bert-gcounter">>}];
headers(_Node, {merge, _Name, _ValueRaw}) ->
    [{<<"Content-Type">>, <<"application/x-bert-gcounter">>}];
headers(_Node, {inc, _Name, _Inc}) ->
    [{<<"text">>, <<"plain">>}];
headers(_Node, {aae, _Data}) ->
    [{<<"Content-Type">>, <<"application/x-bert-aae">>}].

body(_Node, {get_value, _Name}) ->
    [];
body(_Node, {merge, _Name, ValueRaw}) ->
    ValueRaw;
body(_Node, {inc, _Name, Inc}) ->
    integer_to_binary(Inc);
body(_Node, {aae, Data}) ->
    Data.

option(_Node, _Request) ->
    {ok, RequestTimeout} = application:get_env(counterdb, request_timeout),
    [
        {async, true},
        {stream_to, self()},
        {connect_timeout, RequestTimeout},
        {recv_timeout, RequestTimeout}
    ].

port_bin() ->
    {ok, Port} = application:get_env(counterdb, port),
    integer_to_binary(Port).
