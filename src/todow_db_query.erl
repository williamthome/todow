-module(todow_db_query).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-type query() :: string().
-type query_param() :: any().
-type query_params() :: list(query_param()).
-type not_quoted(Type) :: {not_quote, Type}.
-type not_quoted() :: not_quoted(any()).
-type not_quoted_string() :: not_quoted(string()).
-type maybe_quoted() :: not_quoted() | null | string().

-define(should_quote(Value), is_list(Value) orelse is_binary(Value)).

% TODO: Only export usual functions
-export([
    quote/1,
    not_quote/1,
    maybe_quote/1,
    strip/1,
    prepend_symbol/1,
    replace_symbol/3,
    format/1, format/2,
    format_query/1, format_query/2,
    reformat_query/1,
    format_unquoted/1, format_unquoted/2,
    format_clause/1, format_clause/2,
    format_column_value/3,
    format_column_value_with_equal_separator/2,
    format_set/1, format_set/2,
    comma_separated/1,
    not_quoted_comma_separated/1,
    columns_to_string/1,
    values_to_string/1,
    columns_and_values_from_payload/1,
    columns_and_values_to_string_from_payload/1,
    insert_query/4,
    update_query/6
]).

%%------------------------------------------------------------------------------
%% @doc Quotes a string.
%% @end
%%------------------------------------------------------------------------------
% TODO: Change to todow:result()
-spec quote(Value :: any()) -> {ok, string()} | todow_convert_utils:not_string().

quote(Value) ->
    case todow_convert_utils:to_string(Value) of
        {ok, String} -> {ok, lists:concat(["'", String, "'"])};
        Error -> Error
    end.

%%------------------------------------------------------------------------------
%% @doc Wraps the value to prevent quote.
%% @end
%%------------------------------------------------------------------------------
-spec not_quote(Value :: any()) -> not_quoted().

not_quote(Value) -> {not_quote, Value}.

%%------------------------------------------------------------------------------
%% @doc Maybe quote the string.
%% @end
%%------------------------------------------------------------------------------
-spec maybe_quote(Value :: any()) -> maybe_quoted().

maybe_quote({not_quote, Value}) ->
    Value;
maybe_quote(undefined) ->
    null;
maybe_quote(Value) when ?should_quote(Value) ->
    case quote(Value) of
        {ok, Quoted} -> Quoted;
        _ -> todow_convert_utils:must_to_string(Value)
    end;
maybe_quote(Value) ->
    todow_convert_utils:must_to_string(Value).

%%------------------------------------------------------------------------------
%% @doc Maybe quote multiple strings.
%% @end
%%------------------------------------------------------------------------------
-spec maybe_quote_mult(Values :: list()) -> list(maybe_quoted()).

maybe_quote_mult(Values) -> [maybe_quote(Value) || Value <- Values].

%%------------------------------------------------------------------------------
%% @doc Removes all break lines and double spaces.
%% @end
%%------------------------------------------------------------------------------
-spec strip(String :: string()) -> string().

strip(String) ->
    Pattern = "([\r\n]?)\\s+|(^\\s+)|(\\s+$)",
    Replace = " ",
    Options = [global, {return, list}],
    string:trim(re:replace(String, Pattern, Replace, Options)).

%%------------------------------------------------------------------------------
%% @doc Creates an index symbol.
%% @end
%%------------------------------------------------------------------------------
-spec prepend_symbol(Index :: integer()) -> string().

prepend_symbol(Index) when is_integer(Index) andalso Index >= 1 ->
    lists:concat(["$", Index]).

%%------------------------------------------------------------------------------
%% @doc Replaces the index symbol.
%% @end
%%------------------------------------------------------------------------------
-spec replace_symbol(
    Query :: query(), Index :: integer(), Param :: query_param()
) -> string().

replace_symbol(Query, Index, Param) when
    is_list(Query) andalso is_integer(Index)
->
    lists:concat(
        string:replace(Query, prepend_symbol(Index), maybe_quote(Param), all)
    ).

%%------------------------------------------------------------------------------
%% @doc Formats a query.
%% @end
%%------------------------------------------------------------------------------
-spec format(Query :: query()) -> string().

format(Query) -> format(Query, []).

%%------------------------------------------------------------------------------
%% @doc Formats a query.
%% @end
%%------------------------------------------------------------------------------
-spec format(Query :: query(), Params :: query_params()) -> string().

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

%%------------------------------------------------------------------------------
%% @doc Formats a query.
%% @end
%%------------------------------------------------------------------------------
-spec format_query(Query :: query()) -> string().

format_query(Query) -> format_query(Query, []).

%%------------------------------------------------------------------------------
%% @doc Formats a query.
%% @end
%%------------------------------------------------------------------------------
-spec format_query(Query :: query(), Params :: query_params()) -> string().

format_query(Query, Params) -> format(Query, Params) ++ ";".

%%------------------------------------------------------------------------------
%% @doc Reduce and format multiple queries to a single one.
%% @end
%%------------------------------------------------------------------------------
-spec reformat_query(Queries :: list(query())) -> string().

reformat_query(Queries) -> do_reformat_query(Queries, []).

%%------------------------------------------------------------------------------
%% @doc Formats to unquoted string.
%% @end
%%------------------------------------------------------------------------------
-spec format_unquoted(Query :: query()) -> not_quoted_string().

format_unquoted(Query) -> format_unquoted(Query, []).

%%------------------------------------------------------------------------------
%% @doc Formats to unquoted string.
%% @end
%%------------------------------------------------------------------------------
-spec format_unquoted(
    Query :: query(), Params :: query_params()
) -> not_quoted_string().

format_unquoted(Query, Params) -> not_quote(format(Query, Params)).

%%------------------------------------------------------------------------------
%% @doc Formats clause.
%% @end
%%------------------------------------------------------------------------------
-spec format_clause(Query :: query()) -> not_quoted_string().

format_clause(Query) -> format_clause(Query, []).

%%------------------------------------------------------------------------------
%% @doc Formats clause.
%% @end
%%------------------------------------------------------------------------------
-spec format_clause(
    Query :: query(), Params :: query_params()
) -> not_quoted_string().

format_clause(Query, Params) -> format_unquoted(Query, Params).

%%------------------------------------------------------------------------------
%% @doc Formats column and value with a separator.
%% @end
%%------------------------------------------------------------------------------
-spec format_column_value(
    Column :: todow_db_repo:column(),
    Separator :: string(),
    Value :: todow_db_repo:value()
) -> string().

format_column_value(Column, Separator, Value) ->
    format(
        "$1 $2 $3",
        [not_quote(Column), not_quote(Separator), Value]
    ).

%%------------------------------------------------------------------------------
%% @doc Formats column and value with a equal separator.
%% @end
%%------------------------------------------------------------------------------
-spec format_column_value_with_equal_separator(
    Column :: todow_db_repo:column(),
    Value :: todow_db_repo:value()
) -> string().

format_column_value_with_equal_separator(Column, Value) ->
    format_column_value(Column, "=", Value).

%%------------------------------------------------------------------------------
%% @doc Formats payload for set.
%% @end
%%------------------------------------------------------------------------------
-spec format_set(Payload :: todow_db_repo:payload()) -> not_quoted_string().

format_set(Payload) ->
    {Columns, Values} = columns_and_values_from_payload(Payload),
    format_set(Columns, Values).

%%------------------------------------------------------------------------------
%% @doc Formats columns and values for set.
%% @end
%%------------------------------------------------------------------------------
-spec format_set(
    Columns :: todow_db_repo:columns(),
    Values :: todow_db_repo:values()
) -> not_quoted_string().

format_set(Columns, Values) ->
    Set = lists:zipwith(
        fun format_column_value_with_equal_separator/2,
        Columns,
        Values
    ),
    not_quote(comma_separated(Set)).

%%------------------------------------------------------------------------------
%% @doc Creates a comma separated list.
%% @end
%%------------------------------------------------------------------------------
-spec comma_separated(List :: list()) -> string().

comma_separated(List) ->
    string:join(todow_convert_utils:must_to_string_mult(List), ", ").

%%------------------------------------------------------------------------------
%% @doc Creates a comma separated list who never will be quoted.
%% @end
%%------------------------------------------------------------------------------
-spec not_quoted_comma_separated(List :: list()) -> not_quoted_string().

not_quoted_comma_separated(List) -> not_quote(comma_separated(List)).

%%------------------------------------------------------------------------------
%% @doc Convert columns to string.
%% @end
%%------------------------------------------------------------------------------
-spec columns_to_string(List :: list()) -> not_quoted_string().

columns_to_string(Columns) -> not_quoted_comma_separated(Columns).

%%------------------------------------------------------------------------------
%% @doc Convert values to string.
%% @end
%%------------------------------------------------------------------------------
-spec values_to_string(List :: list()) -> maybe_quoted().

values_to_string(Values) ->
    not_quoted_comma_separated(maybe_quote_mult(Values)).

%%------------------------------------------------------------------------------
%% @doc Parses the payload to a tuple of columns and values.
%% @end
%%------------------------------------------------------------------------------
-spec columns_and_values_from_payload(
    Payload :: todow_db_repo:payload()
) -> todow_db_repo:columns_and_values_tuple().

columns_and_values_from_payload(Map) when is_map(Map) ->
    {lists:reverse(maps:keys(Map)), lists:reverse(maps:values(Map))};
columns_and_values_from_payload(Proplist) when is_list(Proplist) ->
    lists:unzip(Proplist);
columns_and_values_from_payload({Columns, Values}) ->
    {Columns, Values}.

%%------------------------------------------------------------------------------
%% @doc Parses the payload to a tuple of columns and values as string.
%% @end
%%------------------------------------------------------------------------------
-spec columns_and_values_to_string_from_payload(
    Payload :: todow_db_repo:payload()
) -> {not_quoted_string(), maybe_quoted()}.

columns_and_values_to_string_from_payload(Payload) ->
    {Columns, Values} = columns_and_values_from_payload(Payload),
    {columns_to_string(Columns), values_to_string(Values)}.

%%------------------------------------------------------------------------------
%% @doc Formats a query for insert data into db.
%% @end
%%------------------------------------------------------------------------------
-spec insert_query(
    Schema :: todow_db_repo:schema(),
    Table :: todow_db_repo:table(),
    Payload :: todow_db_repo:payload(),
    Returning :: todow_db_repo:column()
) -> query().

insert_query(Schema, Table, Payload, Returning) ->
    {Columns, Values} = columns_and_values_to_string_from_payload(Payload),
    Query = "INSERT INTO $1.$2 ($3) VALUES ($4) RETURNING $5",
    Params = [Schema, Table, Columns, Values, Returning],
    format_query(Query, Params).

%%------------------------------------------------------------------------------
%% @doc Formats a query for update db data.
%% @end
%%------------------------------------------------------------------------------
-spec update_query(
    Schema :: todow_db_repo:schema(),
    Table :: todow_db_repo:table(),
    Payload :: todow_db_repo:payload(),
    ClauseQuery :: query(),
    ClauseParams :: query_params(),
    Returning :: todow_db_repo:column()
) -> query().

update_query(Schema, Table, Payload, ClauseQuery, ClauseParams, Returning) ->
    Set = format_set(Payload),
    Clause = format_clause(ClauseQuery, ClauseParams),
    % TODO: Define 'RETURNING' in a fun maybe_returning if returning not undefined
    Query = "UPDATE $1.$2 SET $3 $4 RETURNING $5",
    Params = [Schema, Table, Set, Clause, Returning],
    format_query(Query, Params).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Reformats the query.
%% @end
%%------------------------------------------------------------------------------
-spec do_reformat_query(Queries :: list(query()), Acc :: query()) -> query().

do_reformat_query([], Acc) ->
    Acc;
do_reformat_query([LastQuery], Acc) ->
    Acc ++ format(LastQuery) ++ ";";
do_reformat_query([Query | Queries], Acc) ->
    do_reformat_query(Queries, Acc ++ format(Query) ++ " ").

%%%=============================================================================
%%% Tests
%%%=============================================================================

-ifdef(TEST).

do_columns_and_values_test() ->
    Expected = {[foo, bar], [bar, baz]},
    ?assertEqual(
        Expected,
        columns_and_values_from_payload(#{foo => bar, bar => baz})
    ),
    ?assertEqual(
        Expected,
        columns_and_values_from_payload([{foo, bar}, {bar, baz}])
    ),
    ?assertEqual(
        Expected,
        columns_and_values_from_payload({[foo, bar], [bar, baz]})
    ).

format_column_value_test() ->
    ?assertEqual(
        "foo and a separator between 'bar'",
        format_column_value(foo, "and a separator between", "bar")
    ).

format_column_value_with_equal_separator_test() ->
    ?assertEqual(
        "foo = 'bar'",
        format_column_value_with_equal_separator(foo, "bar")
    ).

format_set_test() ->
    ?assertEqual(
        not_quote("foo = 'bar', bar = 1"),
        format_set([foo, bar], ["bar", 1])
    ).

insert_query_test() ->
    ?assertEqual(
        "INSERT INTO schema.table (foo, bar) VALUES ('bar', 1) RETURNING baz;",
        insert_query(
            schema,
            table,
            #{foo => "bar", bar => 1},
            baz
        )
    ).

update_query_test() ->
    ?assertEqual(
        "UPDATE schema.table SET foo = 'bar', bar = 1 WHERE id = 1 RETURNING baz;",
        update_query(
            schema,
            table,
            #{foo => "bar", bar => 1},
            "WHERE id = $1",
            [1],
            baz
        )
    ).

-endif.
