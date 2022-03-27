% TODO: Change to gen_server behavior
-module(todow_db).

-include("./include/todow.hrl").

-define(SCHEMA, public).
-define(CONTEXT, todow:context()).

-define(should_quote(Value), is_list(Value) orelse is_binary(Value)).

-type id() :: pos_integer().
-type column() :: atom().
-type value() :: any().
-type columns() :: list(column()).
-type values() :: list(value()).
-type columns_and_values_tuple() :: {columns(), values()}.
-type payload() ::
    columns_and_values_tuple()
    | map()
    | list({column(), value()}).
-type not_quoted(Type) :: {not_quote, Type}.
-type not_quoted() :: not_quoted(any()).
-type not_quoted_string() :: not_quoted(string()).
-type maybe_quoted() :: not_quoted() | null | string().
-type query_string() :: string().
-type query_params() :: list().
-type query() :: query_string() | {query_string(), query_params()}.

-type options() :: #{cast => integer}.
-type result(Type) :: {ok, Type} | {error, any()}.
-type result() :: result(any()).
-type result_id() :: result(id()).

-export([
    equery/1, equery/2, equery/3,
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
    schema/0,
    comma_separated/1,
    insert/2, insert/3, insert/4, insert/5,
    update/7,
    update_by_id/4
]).

%%------------------------------------------------------------------------------
%% @doc Executes a query.
%% @end
%%------------------------------------------------------------------------------
-spec equery(Query :: query()) -> any().

equery({Query, Params}) -> equery(Query, Params);
equery(Query) -> equery(Query, []).

%%------------------------------------------------------------------------------
%% @doc Executes a query.
%% @end
%%------------------------------------------------------------------------------
-spec equery(Query :: query_string(), Params :: query_params()) -> any().

% TODO: Remove equery/2 because context must come from a db adapter
equery(Query, Params) -> equery(Query, Params, ?CONTEXT).

%%------------------------------------------------------------------------------
%% @doc Executes a query.
%% @end
%%------------------------------------------------------------------------------
-spec equery(
    Query :: query_string(), Params :: query_params(), Transaction :: any()
) -> any().

equery(Query, Params, Transaction) ->
    % TODO: Change module to be a gen_server and set this as an adapter
    z_db:squery(format_query(Query, Params), Transaction).

%%------------------------------------------------------------------------------
%% @doc Quotes a string.
%% @end
%%------------------------------------------------------------------------------
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
    Query :: query_string(), Index :: integer(), Param :: any()
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

format({Query, Params}) -> format(Query, Params);
format(Query) -> format(Query, []).

%%------------------------------------------------------------------------------
%% @doc Formats a query.
%% @end
%%------------------------------------------------------------------------------
-spec format(
    Query :: query_string(), Params :: query_params()
) -> string().

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

format_query({Query, Params}) -> format_query(Query, Params);
format_query(Query) -> format_query(Query, []).

%%------------------------------------------------------------------------------
%% @doc Formats a query.
%% @end
%%------------------------------------------------------------------------------
-spec format_query(
    Query :: query_string(), Params :: query_params()
) -> string().

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

format_unquoted({Query, Params}) -> format_unquoted(Query, Params);
format_unquoted(Query) -> format_unquoted(Query, []).

%%------------------------------------------------------------------------------
%% @doc Formats to unquoted string.
%% @end
%%------------------------------------------------------------------------------
-spec format_unquoted(
    Query :: query_string(), Params :: query_params()
) -> not_quoted_string().

format_unquoted(Query, Params) -> not_quote(format(Query, Params)).

%%------------------------------------------------------------------------------
%% @doc Formats clause.
%% @end
%%------------------------------------------------------------------------------
-spec format_clause(Query :: query()) -> not_quoted_string().

format_clause({Query, Params}) -> format_clause(Query, Params);
format_clause(Query) -> format_clause(Query, []).

%%------------------------------------------------------------------------------
%% @doc Formats clause.
%% @end
%%------------------------------------------------------------------------------
-spec format_clause(
    Query :: query_string(), Params :: query_params()
) -> not_quoted_string().

format_clause(Query, Params) -> format_unquoted(Query, Params).

%%------------------------------------------------------------------------------
%% @doc Formats column and value with a separator.
%% @end
%%------------------------------------------------------------------------------
-spec format_column_value(
    Column :: atom(), Separator :: string(), Value :: any()
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
    Column :: atom(), Value :: any()
) -> string().

format_column_value_with_equal_separator(Column, Value) ->
    format_column_value(Column, "=", Value).

%%------------------------------------------------------------------------------
%% @doc Formats payload for set.
%% @end
%%------------------------------------------------------------------------------
-spec format_set(Payload :: payload()) -> not_quoted_string().

format_set(Payload) ->
    {Columns, Values} = do_columns_and_values(Payload),
    format_set(Columns, Values).

%%------------------------------------------------------------------------------
%% @doc Formats columns and values for set.
%% @end
%%------------------------------------------------------------------------------
-spec format_set(
    Columns :: columns(), Values :: values()
) -> not_quoted_string().

format_set(Columns, Values) ->
    Set = lists:zipwith(
        fun format_column_value_with_equal_separator/2,
        Columns,
        Values
    ),
    not_quote(comma_separated(Set)).

%%------------------------------------------------------------------------------
%% @doc Returns the db schema.
%% @end
%%------------------------------------------------------------------------------
-spec schema() -> ?SCHEMA.

% TODO: Schema must be defined in state when db will be a gen_server
schema() -> ?SCHEMA.

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
%% @doc Inserts data into db.
%% @end
%%------------------------------------------------------------------------------
-spec insert(Table :: atom(), Payload :: payload()) -> result_id().

insert(Table, Payload) -> insert(?SCHEMA, Table, Payload).

%%------------------------------------------------------------------------------
%% @doc Inserts data into db.
%% @end
%%------------------------------------------------------------------------------
-spec insert(
    Schema :: atom(), Table :: atom(), Payload :: payload()
) -> result_id().

insert(Schema, Table, Payload) ->
    insert(Schema, Table, Payload, id, #{cast => integer}).

%%------------------------------------------------------------------------------
%% @doc Inserts data into db.
%% @end
%%------------------------------------------------------------------------------
-spec insert(
    Table :: atom(),
    Payload :: payload(),
    Returning :: column(),
    Options :: options()
) -> result().

insert(Table, Payload, Returning, Options) ->
    insert(?SCHEMA, Table, Payload, Returning, Options).

%%------------------------------------------------------------------------------
%% @doc Inserts data into db.
%% @end
%%------------------------------------------------------------------------------
-spec insert(
    Schema :: atom(),
    Table :: atom(),
    Payload :: payload(),
    Returning :: column(),
    Options :: options()
) -> result().

insert(Schema, Table, Payload, Returning, Options) ->
    Query = insert_query(Schema, Table, Payload, Returning),
    Result = equery(Query),
    process_result(Result, Options).

%%------------------------------------------------------------------------------
%% @doc Updates db data.
%% @end
%%------------------------------------------------------------------------------
-spec update(
    Schema :: atom(),
    Table :: atom(),
    Payload :: payload(),
    ClauseQuery :: query_string(),
    ClauseParams :: query_params(),
    Returning :: column(),
    Options :: options()
) -> result().

update(Schema, Table, Payload, ClauseQuery, ClauseParams, Returning, Options) ->
    Query = update_query(
        Schema, Table, Payload, ClauseQuery, ClauseParams, Returning
    ),
    Result = equery(Query),
    process_result(Result, Options).

%%------------------------------------------------------------------------------
%% @doc Updates db data by id.
%% @end
%%------------------------------------------------------------------------------
-spec update_by_id(
    Schema :: atom(),
    Table :: atom(),
    Id :: id(),
    Payload :: payload()
) -> result().

update_by_id(Schema, Table, Id, Payload) ->
    ClauseQuery = "WHERE id = $1",
    ClauseParams = [Id],
    Returning = id,
    Options = #{cast => integer},
    update(Schema, Table, Payload, ClauseQuery, ClauseParams, Returning, Options).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Reformats the query.
%% @end
%%------------------------------------------------------------------------------
-spec do_reformat_query(Queries :: list(string()), Acc :: string()) -> string().

do_reformat_query([], Acc) ->
    Acc;
do_reformat_query([LastQuery], Acc) ->
    Acc ++ format(LastQuery) ++ ";";
do_reformat_query([Query | Queries], Acc) ->
    do_reformat_query(Queries, Acc ++ format(Query) ++ " ").

%%------------------------------------------------------------------------------
%% @doc Transform to insert result.
%% @end
%%------------------------------------------------------------------------------
-spec process_result(Result :: any(), Options :: options()) -> result().

% TODO: This converts Zotonic to expected result, but must come from an adapter
process_result({ok, 1, _Columns, [{Value}]}, Options) ->
    {ok, process_result_options(Value, Options)};
process_result({error, _} = Error, _Options) ->
    Error.

%%------------------------------------------------------------------------------
%% @doc Transform to insert result by options.
%% @end
%%------------------------------------------------------------------------------
-spec process_result_options(Value :: any(), Options :: options()) -> any().

process_result_options(Value, #{cast := integer}) ->
    todow_convert_utils:must_to_integer(Value);
process_result_options(Value, #{}) ->
    Value.

%%------------------------------------------------------------------------------
%% @doc Parses the payload to a tuple of columns and values.
%% @end
%%------------------------------------------------------------------------------
-spec do_columns_and_values(Payload :: payload()) -> columns_and_values_tuple().

do_columns_and_values(Map) when is_map(Map) ->
    {lists:reverse(maps:keys(Map)), lists:reverse(maps:values(Map))};
do_columns_and_values(Proplist) when is_list(Proplist) ->
    lists:unzip(Proplist);
do_columns_and_values({Columns, Values}) ->
    {Columns, Values}.

%%------------------------------------------------------------------------------
%% @doc Parses the payload to a tuple of columns and values as string.
%% @end
%%------------------------------------------------------------------------------
-spec do_columns_and_values_as_string(
    Payload :: payload()
) -> {not_quoted_string(), maybe_quoted()}.

do_columns_and_values_as_string(Payload) ->
    {Columns, Values} = do_columns_and_values(Payload),
    {columns_to_string(Columns), values_to_string(Values)}.

%%------------------------------------------------------------------------------
%% @doc Formats a query for insert data into db.
%% @end
%%------------------------------------------------------------------------------
-spec insert_query(
    Schema :: atom(),
    Table :: atom(),
    Payload :: payload(),
    Returning :: column()
) -> string().

insert_query(Schema, Table, Payload, Returning) ->
    {Columns, Values} = do_columns_and_values_as_string(Payload),
    Query = "INSERT INTO $1.$2 ($3) VALUES ($4) RETURNING $5",
    Params = [Schema, Table, Columns, Values, Returning],
    format_query(Query, Params).

%%------------------------------------------------------------------------------
%% @doc Formats a query for update db data.
%% @end
%%------------------------------------------------------------------------------
-spec update_query(
    Schema :: atom(),
    Table :: atom(),
    Payload :: payload(),
    ClauseQuery :: query_string(),
    ClauseParams :: query_params(),
    Returning :: column()
) -> string().

update_query(Schema, Table, Payload, ClauseQuery, ClauseParams, Returning) ->
    Set = format_set(Payload),
    Clause = format_clause(ClauseQuery, ClauseParams),
    % TODO: Define 'RETURNING' in a fun maybe_returning if returning not undefined
    Query = "UPDATE $1.$2 SET $3 $4 RETURNING $5",
    Params = [Schema, Table, Set, Clause, Returning],
    format_query(Query, Params).

%%%=============================================================================
%%% Tests
%%%=============================================================================

-ifdef(TEST).

do_columns_and_values_test() ->
    Expected = {[foo, bar], [bar, baz]},
    ?assertEqual(
        Expected,
        do_columns_and_values(#{foo => bar, bar => baz})
    ),
    ?assertEqual(
        Expected,
        do_columns_and_values([{foo, bar}, {bar, baz}])
    ),
    ?assertEqual(
        Expected,
        do_columns_and_values({[foo, bar], [bar, baz]})
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
