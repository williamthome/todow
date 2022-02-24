-module(todow_db).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([
  query/1, query/2,
  quote/1, not_quote/1, maybe_quote/1,
  strip/1,
  prepend_symbol/1, replace_symbol/3,
  format/2
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

%%====================================================================
%% Tests
%%====================================================================

-ifdef(TEST).

quote_test() ->
  ?assertEqual({ok, "'foo'"}, quote("foo")),
  ?assertEqual({ok, "'foo'"}, quote(<<"foo">>)),
  ?assertEqual({ok, "'foo'"}, quote(foo)),
  ?assertEqual({ok, "'0'"}, quote(0)).

maybe_quote_test() ->
  ?assertEqual("'foo'", maybe_quote("foo")),
  ?assertEqual("'foo'", maybe_quote(<<"foo">>)),
  ?assertEqual("foo", maybe_quote(foo)),
  ?assertEqual("0", maybe_quote(0)),
  ?assertEqual(foo, maybe_quote(not_quote(foo))),
  ?assertEqual(0, maybe_quote(not_quote(0))).

strip_test() ->
  ?assertEqual("foo bar", strip("    foo\n     bar\r\n     ")).

prepend_symbol_test() ->
  ?assertEqual("$1", prepend_symbol(1)).

symbol_to_param_test() ->
  ?assertEqual("foo", replace_symbol("$1", 1, foo)),
  ?assertEqual("'bar'", replace_symbol("$1", 1, "bar")).

format_test() ->
  ?assertEqual(
    "SELECT * FROM foo WHERE bar = 'baz'",
    format(
      "   SELECT  *     FROM  \r\n  $1 WHERE $2 = $3 \n  ",
      [foo, bar, "baz"]
    )
  ).

-endif.
