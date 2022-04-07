-module(zotonic_db_adapter).
-behaviour(todow_db_adapter).

-include("../../include/todow_db.hrl").

-define(CONTEXT, todow:context()).

-export([
    get_connection/0,
    equery/2,
    transaction/2
]).

%%------------------------------------------------------------------------------
%% @doc Returns the default context as connection.
%% @end
%%------------------------------------------------------------------------------
-spec get_connection() -> connection().

get_connection() -> ?CONTEXT.

%%------------------------------------------------------------------------------
%% @doc Executes a query.
%% @end
%%------------------------------------------------------------------------------
-spec equery(
    Connection :: connection(),
    Query :: string()
) -> result().

equery(Connection, Query) ->
    Result = z_db:squery(Query, Connection),
    to_db_result(Result).

%%------------------------------------------------------------------------------
%% @doc Executes a function in a transaction.
%% @end
%%------------------------------------------------------------------------------

-spec transaction(
    Connection :: connection(),
    Fun :: transaction_fun()
) -> result().

transaction(Connection, Fun) ->
    Result = z_db:transaction(Fun, Connection),
    to_db_result(Result).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Transforms to expected db result.
%% @end
%%------------------------------------------------------------------------------
-spec to_db_result(Result :: any()) -> result().

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
    Error;
to_db_result({ok, _} = Result) ->
    Result;
to_db_result(Result) ->
    {ok, Result}.
