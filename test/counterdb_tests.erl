-module(counterdb_tests).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(N, 9).
-define(KEY, <<"quorum">>).

setup() ->
    application:load(counterdb),
    application:set_env(counterdb, max_wait_timeout, 10000),

    NTs =
        [ begin
            {N, ets:new(?MODULE, [public])}
        end || N <- lists:seq(1, ?N) ],

    meck:new(counterdb_cluster),
    meck:expect(counterdb_cluster, get, fun() ->
        [integer_to_binary(N) || N <- lists:seq(1, ?N)]
    end),

    meck:new(counterdb_storage, [non_strict]),
    meck:expect(counterdb_storage, t, fun () ->
        case erlang:get(n) of
            undefined ->
                throw(n);
            N ->
                lists:keyfind(N, 1, NTs)
        end
    end),
    meck:expect(counterdb_storage, with, fun (N, F) ->
        O = erlang:put(n, N),
        R = F(),
        erlang:put(n, O),
        R
    end),
    meck:expect(counterdb_storage, get, fun (Name) ->
        {_, T} = counterdb_storage:t(),
        case ets:lookup(T, Name) of
            [] -> not_found;
            [{_, V}] -> V
        end
    end),
    meck:expect(counterdb_storage, update, fun (Name, {M, F, A}) ->
        {ServerId, T} = counterdb_storage:t(),
        Value = counterdb_storage:get(Name),
        Value0 = apply(M, F, [ServerId, Value|A]),
        true = ets:insert(T, {Name, Value0}),
        Value0
    end),
    meck:expect(counterdb_storage, snapshot, fun () ->
        {_, T} = counterdb_storage:t(),
        ets:foldl(fun ({K, V}, Acc) ->
            Acc#{K => V}
        end, #{}, T)
    end),

    meck:new(hackney),
    meck:expect(hackney, request, fun(Method, Url, _, Body, _) ->
        % each 4th request will be failed
        case rand:uniform() > 0.25 of
            true ->
                {ok, {_, _, Host, _, Path, _}} = http_uri:parse(binary_to_list(Url)),
                [_, NameStr, _] = string:tokens(Path, "/"),
                N = list_to_integer(Host),
                Name = list_to_binary(NameStr),
                {ok, _, Result} =
                    counterdb_storage:with(N, fun () ->
                        case Method of
                            'get' ->
                                counterdb:get_value(Name);
                            'post' ->
                                Value = binary_to_term(Body),
                                counterdb:merge_value(Name, Value)
                        end
                    end),
                Ref = make_ref(),
                self() ! {hackney_response, Ref, {status, 200, <<"OK">>}},
                self() ! {hackney_response, Ref, Result},
                self() ! {hackney_response, Ref, done},
                {ok, Ref};
            false ->
                {error, failed}
        end
    end),

    NTs.

cleanup(NTs) ->
    [ ets:delete(T) || {_, T} <- NTs ],
    meck:unload(hackney),
    meck:unload(counterdb_cluster),
    meck:unload(counterdb_storage),
    application:unload(counterdb).

unique_key() ->
    N = erlang:unique_integer([positive, monotonic]),
    NStr = integer_to_binary(N),
    <<?KEY/binary, "-", NStr/binary>>.

proper_small_test_() ->
    PropErOpts = [
        {to_file, user},
        {max_size, 64},
        {numtests, 128}
    ],
    {setup, fun setup/0, fun cleanup/1,
        {timeout, 60000,
            ?_assertEqual([], proper:module(?MODULE, PropErOpts))}
    }.

-ifdef(BIGTEST).

proper_big_test_() ->
    PropErOpts = [
        {to_file, user},
        {max_size, 1024},
        {numtests, 1024}
    ],
    {setup, fun setup/0, fun cleanup/1,
        {timeout, 60000,
            ?_assertEqual([], proper:module(?MODULE, PropErOpts))}
    }.

-endif.

prop_counterdb_quorum() ->
    Quorum = trunc(?N / 2) + 1,
    ?FORALL(Actions, gen_list_actions(),
        begin
            Key = unique_key(),
            Sum = lists:foldl(fun ({N, I}, Acc) ->
                counterdb_storage:with(N, fun () ->
                    counterdb:inc_value(Quorum, Key, I)
                end),
                Acc + I
            end, 0, Actions),
            {ok, Value, _ValueBin} =
                counterdb:get_consistent_value(Quorum, Key),
            Sum =:= Value
        end
    ).


gen_server() ->
    proper_types:choose(1, ?N).

gen_inc() ->
    proper_types:choose(0, 100).

gen_list_actions() ->
    proper_types:list(
        ?LET(N, gen_server(),
        ?LET(I, gen_inc(),
            {N, I}
        ))
    ).
