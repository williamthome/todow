-module(zotonic_db_adapter).
-behaviour(todow_db_adapter).

-define(CONTEXT, todow:context()).

-type connection() :: z:context().

-export([
    get_connection/0,
    equery/1,
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
-spec equery(Query :: string()) -> todow_db:result().

equery(Query) ->
    Result = z_db:squery(Query, ?CONTEXT),
    to_db_result(Result).

%%------------------------------------------------------------------------------
%% @doc Executes a function in a transaction.
%% @end
%%------------------------------------------------------------------------------

-spec transaction(z:context(), todow_db:transaction_fun()) -> todow_db:result().

transaction(Context, Fun) ->
    Result = z_db:transaction(Fun, Context),
    to_db_result(Result).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Transforms to expected db result.
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
    Error;
to_db_result(Result) ->
    {ok, Result}.
