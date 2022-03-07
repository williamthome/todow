-module(todow_db_test).

-import(todow_db, [
    quote/1,
    not_quote/1,
    maybe_quote/1,
    strip/1,
    prepend_symbol/1,
    replace_symbol/3,
    format/1, format/2,
    format_query/1, format_query/2,
    reformat_query/1
]).

-include_lib("eunit/include/eunit.hrl").

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

format_query_test() ->
    % format_query/1
    ?assertEqual(
        "SELECT * FROM foo WHERE bar = 'baz';",
        format_query(
            "   SELECT  *     FROM  \r\n  foo WHERE bar = 'baz' \n  "
        )
    ),
    % format_query/2
    ?assertEqual(
        "SELECT * FROM foo WHERE bar = 'baz';",
        format_query(
            "   SELECT  *     FROM  \r\n  $1 WHERE $2 = $3 \n  ",
            [foo, bar, "baz"]
        )
    ).

reformat_query_test() ->
    ?assertEqual(
        "SELECT * FROM foo WHERE bar = 'baz';",
        reformat_query(
            [
                "   SELECT  *     ",
                "   FROM  \r\n  foo  ",
                "      WHERE bar = 'baz' \n  "
            ]
        )
    ).
