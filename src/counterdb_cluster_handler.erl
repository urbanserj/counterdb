-module(counterdb_cluster_handler).

-export([
    init/3,
    rest_init/2,
    allowed_methods/2,
    content_types_accepted/2,
    content_types_provided/2,
    process/2
]).

init(_Transport, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.

rest_init(Req, Opts) ->
    {ok, Req, Opts}.

allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
    {[
        {{<<"application">>, <<"json">>, []}, process}
    ], Req, State}.

content_types_provided(Req, State) ->
    {[
        {{<<"text">>, <<"plain">>, []}, process}
    ], Req, State}.

process(Req, State) ->
    {ok, Body, Req0} = cowboy_req:body(Req),
    try get_actors(jsx:decode(Body, [return_maps])) of
        Actors ->
            case counterdb_cluster:set(Actors) of
                ok ->
                    {true, Req0, State};
                {error, _} ->
                    halt_reply(409, Req0, State)
            end
    catch
        Type:Error ->
            lager:error(
                "Config processing error: "
                "type=~p error=~p data=\"~s\"~n", [Type, Error, Body]),
            halt_reply(400, Req0, State)
    end.

get_actors(#{<<"actors">> := List = [_|_]}) ->
    case lists:all(fun is_binary/1, List) of
        true -> List;
        false -> error(badarg)
    end;
get_actors(_) ->
    error(badarg).

halt_reply(Status, Req, State) ->
    {ok, Req0} = cowboy_req:reply(Status, Req),
    {halt, Req0, State}.
