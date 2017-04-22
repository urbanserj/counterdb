-module(counterdb_aae_handler).

-export([
    init/3,
    rest_init/2,
    allowed_methods/2,
    content_types_accepted/2,
    content_types_provided/2,

    process_aae/2
]).

init(_Transport, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.

rest_init(Req, Opts) ->
    {ok, Req, Opts}.

allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
    {[
        {{<<"application">>, <<"x-bert-aae">>, []}, process_aae}
    ], Req, State}.

content_types_provided(Req, State) ->
    {[
        {{<<"application">>, <<"x-bert-aae">>, []}, process_aae}
    ], Req, State}.

process_aae(Req, State) ->
    {ok, Body, Req0} = get_full_body(Req),
    try binary_to_term(Body) of
        Data ->
            ok = counterdb_aae:snapshot(Data),
            {true, Req0, State}
    catch
        error:badarg ->
            Req1 = cowboy_req:reply(400, Req0),
            {halt, Req1, State}
    end.

-spec get_full_body(Req) -> {ok, Data, Req}
    when Data :: binary(), Req :: cowboy_req:req().
get_full_body(Req) ->
    % Dialyzer doesn't like [{length, infinity}]
    % so I wrote this code.
    get_full_body(<<>>, Req).

-spec get_full_body(Data, Req) -> {ok, Data, Req}
    when Data :: binary(), Req :: cowboy_req:req().
get_full_body(Buf, Req) ->
    case cowboy_req:body(Req) of
        {ok, Data, Req0} ->
            {ok, <<Buf/binary, Data/binary>>, Req0};
        {more, Data, Req0} ->
            get_full_body(<<Buf/binary, Data/binary>>, Req0)
    end.
