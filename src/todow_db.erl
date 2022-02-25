-module(todow_db).

-export([
  equery/1, equery/2,
  quote/1, not_quote/1, maybe_quote/1,
  strip/1,
  prepend_symbol/1, replace_symbol/3,
  format/1, format/2,
  format_query/1, format_query/2,
  reformat_query/1
]).

-define(should_quote(Value), is_list(Value) orelse is_binary(Value)).

equery(Query, Args) -> z_db:equery(Query, Args, todow:context()).
equery(Query) -> equery(Query, []).

-spec quote(Value :: any()) -> {ok, string()} | todow_convert_utils:not_string().

quote(Value) ->
  case todow_convert_utils:to_string(Value) of
    {ok, String} -> {ok, lists:concat(["'", String, "'"])};
    Error -> Error
  end.

-spec not_quote(Value :: any()) -> {not_quote, any()}.

not_quote(Value) -> {not_quote, Value}.

-spec maybe_quote(Value :: any()) -> {not_quote, any()} | null | string() | any().

maybe_quote({not_quote, Value}) -> Value;
maybe_quote(undefined) -> null;
maybe_quote(Value) when ?should_quote(Value) ->
  case quote(Value) of
    {ok, Quoted} -> Quoted;
    _ -> todow_convert_utils:maybe_to_string(Value)
  end;
maybe_quote(Value) -> todow_convert_utils:maybe_to_string(Value).

-spec strip(String :: string()) -> string().

strip(String) ->
  Pattern = "([\r\n]?)\\s+|(^\\s+)|(\\s+$)",
  Replace = " ",
  Options = [global, {return, list}],
  string:trim(re:replace(String, Pattern, Replace, Options)).

-spec prepend_symbol(Index :: integer()) -> string().

prepend_symbol(Index) when is_integer(Index) andalso Index >= 1 ->
  lists:concat(["$", Index]).

-spec replace_symbol(Query :: string(), Index :: integer(), Param :: any()) -> string().

replace_symbol(Query, Index, Param) when is_list(Query) andalso is_integer(Index) ->
  lists:concat(string:replace(Query, prepend_symbol(Index), maybe_quote(Param), all)).

-spec format(Query :: string() | {string(), list()}) -> string().

format({Query, Params}) -> format(Query, Params);

format(Query) -> format(Query, []).

-spec format(Query :: string(), Params :: list()) -> string().

format(Query, Params) ->
  StrippedQuery = strip(Query),
  {FormattedQuery, _Index} = lists:foldl(
    fun(Param, {QueryAcc, Index}) ->
      {replace_symbol(QueryAcc, Index, Param), Index + 1}
    end,
    {StrippedQuery, 1},
    Params
  ),
  FormattedQuery.

-spec format_query(Query :: string() | {string(), list()}) -> string().

format_query({Query, Params}) -> format_query(Query, Params);

format_query(Query) -> format_query(Query, []).

-spec format_query(Query :: string(), Params :: list()) -> string().

format_query(Query, Params) -> format(Query, Params) ++ ";".

%% @doc Reduce and format multiple queries to a single one

-spec reformat_query(Queries :: list(string() | {string(), list()})) -> string().

reformat_query(Queries) -> do_reformat_query(Queries, []).

%%====================================================================
%% Internal functions
%%====================================================================

do_reformat_query([], Acc) -> Acc;

do_reformat_query([LastQuery], Acc) -> Acc ++ format(LastQuery) ++ ";";

do_reformat_query([Query | Queries], Acc) ->
   do_reformat_query(Queries, Acc ++ format(Query) ++ " ").
