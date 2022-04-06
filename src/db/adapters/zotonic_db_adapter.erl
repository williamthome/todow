-module(zotonic_db_adapter).
-behaviour(todow_db_adapter).

-export([ equery/1 ]).

-define(CONTEXT, todow:context()).

%%------------------------------------------------------------------------------
%% @doc Executes a query.
%% @end
%%------------------------------------------------------------------------------
-spec equery(Query :: string()) -> any().

equery(Query) ->
    Result = z_db:squery(Query, ?CONTEXT),
    to_db_result(Result).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Transform to insert result.
%% @end
%%------------------------------------------------------------------------------
-spec to_db_result(Result :: any()) -> todow_db:result().

to_db_result({ok, _, _Column, [{Value}]}) ->
    {ok, Value};
to_db_result({ok, _, _Columns, Values}) ->
    {ok, Values};
to_db_result({ok, _Column, [{Value}]}) ->
    {ok, Value};
to_db_result({ok, _Columns, Values}) ->
    {ok, Values};
to_db_result({error, {error, error, _Code, Reason, Msg, _Debug}}) ->
    {error, {Reason, Msg}};
to_db_result({error, _} = Error) ->
    Error.
