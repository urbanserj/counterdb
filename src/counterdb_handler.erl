-module(counterdb_handler).

-export([
    init/3,
    rest_init/2,
    service_available/2,
    allowed_methods/2,
    content_types_accepted/2,
    content_types_provided/2,

    process_update/2,
    process_merge/2,
    get_value/2,
    get_value_raw/2
]).

init(_Transport, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.

rest_init(Req, Opts) ->
    {ok, Req, Opts}.

service_available(Req, State) ->
    {case counterdb_cluster:get() of
        undefined -> false;
        _List -> true
    end, Req, State}.

allowed_methods(Req, State) ->
    {case State of
        value -> [<<"GET">>];
        consistent_value -> [<<"GET">>];
        update -> [<<"POST">>];
        merge -> [<<"POST">>]
    end, Req, State}.

content_types_accepted(Req, update) ->
    {[
        {'*', process_update}
    ], Req, update};
content_types_accepted(Req, merge) ->
    {[
        {{<<"application">>, <<"x-bert-gcounter">>, []}, process_merge}
    ], Req, merge}.

content_types_provided(Req, merge) ->
    {[
        {{<<"application">>, <<"x-bert-gcounter">>, []}, process_merge}
    ], Req, merge};
content_types_provided(Req, State) ->
    {[
        {{<<"text">>, <<"plain">>, []}, get_value},
        {{<<"application">>, <<"x-bert-gcounter">>, []}, get_value_raw}
    ], Req, State}.

process_update(Req, State) ->
    {ok, Body, Req0} = cowboy_req:body(Req),
    {Name, Req1} = cowboy_req:binding(name, Req0),
    {Wait, Req2} = qs_val_wait(Req1),
    try {Name, increment(Body)} of
        {<<$$, _/binary>>, _} ->
            % internal use only
            {ok, Req3} = cowboy_req:reply(403, Req2),
            {halt, Req3, State};
        {_, Increment} ->
            lager:debug("Increment: ~p ~p", [Name, Increment]),
            {Res, Value, _} = counterdb:inc_value(Wait, Name, Increment),
            {ok, Req3} =
                cowboy_req:reply(
                    case Res of
                        wait_timeout -> 202;
                        ok -> 200
                    end, [], integer_to_binary(Value), Req2),
            {halt, Req3, State}
    catch
        error:badarg ->
            {ok, Req3} = cowboy_req:reply(400, Req2),
            {halt, Req3, State}
    end.

process_merge(Req, State) ->
    {ok, Body, Req0} = cowboy_req:body(Req),
    {Name, Req1} = cowboy_req:binding(name, Req0),
    {ok, _ResultValue, Result} = counterdb:merge_value(Name, Body),
    Req2 =
        case Result of
            Body -> Req1;
            Result ->
                cowboy_req:set_resp_body(Result, Req1)
        end,
    {true, Req2, State}.

get_value(Req, State) ->
    get_value(Req, State, text).

get_value_raw(Req, State) ->
    get_value(Req, State, raw).

get_value(Req, Type, Output) ->
    {Name, Req0} = cowboy_req:binding(name, Req),
    {Wait, Req1} = qs_val_wait(Req0),
    {Res, Value, ValueRaw} =
        case Type of
            value ->
                counterdb:get_value(Name);
            consistent_value ->
                counterdb:get_consistent_value(Wait, Name)
        end,
    Body =
        case Output of
            text -> integer_to_binary(Value);
            raw  -> ValueRaw
        end,
    {ok, Req2} =
        cowboy_req:reply(
            case Res of
                wait_timeout -> 408;
                ok -> 200
            end, [], Body, Req1),
    {halt, Req2, Type}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec increment(binary()) -> non_neg_integer().
increment(Bin) when is_binary(Bin) ->
    case binary_to_integer(Bin) of
        Inc when Inc >= 0 ->
            Inc;
        _Inc ->
            error(badarg)
    end.

-spec qs_val_wait(Req) -> {counterdb:wait(), Req}
    when Req :: cowboy_req:req().
qs_val_wait(Req) ->
    {Wait, Req0} = cowboy_req:qs_val(<<"wait">>, Req),
    All = length(counterdb_cluster:get()),
    case Wait of
        undefined ->
            {undefined, Req0};
        <<"quorum">> ->
            {trunc(All / 2) + 1, Req0};
        <<"all">> ->
            {All, Req0};
        Wait ->
            try binary_to_integer(Wait) of
                N when N >= 1, N =< All ->
                    {N, Req0};
                _ ->
                    {undefined, Req0}
            catch
                _:_ ->
                    {undefined, Req0}
            end
    end.
