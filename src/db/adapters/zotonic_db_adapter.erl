-module(zotonic_db_adapter).
-behaviour(todow_db_adapter).

-export([
  equery/1,
  process_result/2
]).

-define(CONTEXT, todow:context()).

%%------------------------------------------------------------------------------
%% @doc Executes a query.
%% @end
%%------------------------------------------------------------------------------
-spec equery(Query :: string()) -> any().

equery(Query) -> z_db:squery(Query, ?CONTEXT).

%%------------------------------------------------------------------------------
%% @doc Transform to insert result.
%% @end
%%------------------------------------------------------------------------------
-spec process_result(
    Result :: any(), Options :: todow_db:options()
) -> todow_db:result().

process_result({ok, 1, _Columns, [{Value}]}, Options) ->
    {ok, process_result_options(Value, Options)};
process_result({error, _} = Error, _Options) ->
    Error.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Transform to insert result by options.
%% @end
%%------------------------------------------------------------------------------
-spec process_result_options(
    Value :: any(), Options :: todow_db:options()
) -> any().

process_result_options(Value, #{cast := integer}) ->
    todow_convert_utils:must_to_integer(Value);
process_result_options(Value, #{}) ->
    Value.
