-module(todow_db).

-export([
  query/1, query/2,
  quote/1, not_quote/1, maybe_quote/1,
  strip/1,
  prepend_symbol/1, replace_symbol/3,
  format/1, format/2,
  format_sql/1, format_sql/2,
  join_sqls/1
]).

-define(should_quote(Value), is_list(Value) orelse is_binary(Value)).

query(Sql, Args) -> z_db:equery(Sql, Args, todow:context()).
query(Sql) -> query(Sql, []).

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

-spec replace_symbol(Sql :: string(), Index :: integer(), Param :: any()) -> string().

replace_symbol(Sql, Index, Param) when is_list(Sql) andalso is_integer(Index) ->
  lists:concat(string:replace(Sql, prepend_symbol(Index), maybe_quote(Param), all)).

-spec format(Sql :: string() | {string(), list()}) -> string().

format({Sql, Params}) -> format(Sql, Params);

format(Sql) -> format(Sql, []).

-spec format(Sql :: string(), Params :: list()) -> string().

format(Sql, Params) ->
  StrippedSql = strip(Sql),
  {FormattedSql, _Index} = lists:foldl(
    fun(Param, {SqlAcc, Index}) ->
      {replace_symbol(SqlAcc, Index, Param), Index + 1}
    end,
    {StrippedSql, 1},
    Params
  ),
  FormattedSql.

-spec format_sql(Sql :: string() | {string(), list()}) -> string().

format_sql({Sql, Params}) -> format_sql(Sql, Params);

format_sql(Sql) -> format_sql(Sql, []).

-spec format_sql(Sql :: string(), Params :: list()) -> string().

format_sql(Sql, Params) -> format(Sql, Params) ++ ";".

-spec join_sqls(Sqls :: list(string() | {string(), list()})) -> string().

join_sqls(Sqls) -> do_join_sqls(Sqls, []).

%%====================================================================
%% Internal functions
%%====================================================================

do_join_sqls([], Acc) -> Acc;

do_join_sqls([LastSql], Acc) -> Acc ++ format(LastSql) ++ ";";

do_join_sqls([Sql | Sqls], Acc) ->
   do_join_sqls(Sqls, Acc ++ format(Sql) ++ " ").
