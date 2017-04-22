-module(counterdb_aae_tests).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(N, 9).
-define(KEY, <<"aaetest">>).

setup() ->
    application:load(counterdb),
    application:set_env(counterdb, max_wait_timeout, 300000),
    application:set_env(counterdb, aae_timeout, 250),

    meck:new(counterdb_cluster),
    meck:expect(counterdb_cluster, get, fun() ->
        [integer_to_binary(N) || N <- lists:seq(1, ?N)]
    end),

    NPTs =
        [ begin
            {ok, Pid} = gen_server:start_link(counterdb_aae, [N], []),
            SPid = spawn_link(fun sloop/0),
            {N, Pid, SPid, ets:new(?MODULE, [public])}
        end || N <- lists:seq(1, ?N) ],

    meck:new(counterdb_storage, [non_strict]),
    meck:expect(counterdb_storage, t, fun () ->
        case erlang:get(n) of
            undefined ->
                lists:keyfind(self(), 2, NPTs);
            N ->
                lists:keyfind(N, 1, NPTs)
        end
    end),
    meck:expect(counterdb_storage, with, fun (N, F) ->
        O = erlang:put(n, N),
        R = F(),
        erlang:put(n, O),
        R
    end),
    meck:expect(counterdb_storage, get, fun (Name) ->
        {_, _, _, T} = counterdb_storage:t(),
        case ets:lookup(T, Name) of
            [] -> not_found;
            [{_, V}] -> V
        end
    end),
    meck:expect(counterdb_storage, update, fun (Name, {M, F, A}) ->
        {ServerId, _, S, T} = counterdb_storage:t(),
        Fun =
            fun () ->
                Value = counterdb_storage:get(Name),
                Value0 = apply(M, F, [ServerId, Value|A]),
                true = ets:insert(T, {Name, Value0}),
                Value0
            end,
        S ! {self(), ServerId, Fun},
        receive {S, R} -> R end
    end),
    meck:expect(counterdb_storage, snapshot, fun () ->
        {_, _, _, T} = counterdb_storage:t(),
        ets:foldl(fun ({K, V}, Acc) ->
            Acc#{K => V}
        end, #{}, T)
    end),
    meck:expect(counterdb_storage, delete_all_objects, fun () ->
        {ServerId, _, S, T} = counterdb_storage:t(),
        Fun =
            fun () ->
                true = ets:delete_all_objects(T)
            end,
        S ! {self(), ServerId, Fun},
        receive {S, R} -> R end
    end),

    meck:new(hackney),
    meck:expect(hackney, request, fun('post', Url, _, Body, _) ->
        % each 4th request will be failed
        case rand:uniform() > 0.25 of
            true ->
                {ok, {_, _, Host, _, Path, _}} = http_uri:parse(binary_to_list(Url)),
                N = list_to_integer(Host),
                case Path of
                    "/aae" ->
                        counterdb_storage:with(N, fun () ->
                            Snapshot = binary_to_term(Body),
                            counterdb_aae:snapshot(Snapshot)
                        end),
                        Ref = make_ref(),
                        self() ! {hackney_response, Ref, {status, 204, <<"No content">>}},
                        self() ! {hackney_response, Ref, done},
                        {ok, Ref};
                    Path ->
                        [_, NameStr] = string:tokens(Path, "/"),
                        Name = list_to_binary(NameStr),
                        I = binary_to_integer(Body),
                        {ok, Value, _} =
                            counterdb_storage:with(N, fun () ->
                                counterdb:inc_value(Name, I)
                            end),
                        Ref = make_ref(),
                        self() ! {hackney_response, Ref, {status, 200, <<"OK">>}},
                        self() ! {hackney_response, Ref, integer_to_binary(Value)},
                        self() ! {hackney_response, Ref, done},
                        {ok, Ref}
                end;
            false ->
                {error, failed}
        end
    end),
    NPTs.

cleanup(NPTs) ->
    meck:unload(hackney),
    meck:unload(counterdb_storage),
    application:unload(counterdb),
    [ begin
        erlang:unlink(P),
        erlang:exit(P, kill),
        erlang:unlink(S),
        erlang:exit(S, kill),
        ets:delete(T)
    end || {_, P, S, T} <- NPTs],
    meck:unload(counterdb_cluster).

cleanup_storages() ->
    lists:foreach(fun (N) ->
        counterdb_storage:with(N, fun () ->
            true = counterdb_storage:delete_all_objects()
        end)
    end, lists:seq(1, ?N)).

sloop() ->
    receive
        {Pid, ServerId, Fun} ->
            erlang:put(n, ServerId),
            Pid ! {self(), Fun()},
            sloop()
    end.

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

prop_counterdb_aae() ->
    ?FORALL({C, Actions}, gen_server_actions(),
        try
            Key = unique_key(),
            Sum = lists:foldl(fun ({N, I}, Acc) ->
                counterdb_storage:with(N, fun () ->
                    counterdb:inc_value(Key, I)
                end),
                Acc + I
            end, 0, Actions),
            {ok, Value, _ValueBin} =
                counterdb_storage:with(C, fun () ->
                    counterdb:get_consistent_value(Key)
                end),
            Sum =:= Value
        after
            % It's here only for speed
            cleanup_storages()
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

gen_server_actions() ->
    ?LET(X, gen_server(),
    ?LET(A, gen_list_actions(),
        {X, A}
    )).
