-module(todow_db).

-type not_quote(Type) :: {not_quote, Type}.
-type not_quote() :: not_quote(any()).
-type maybe_quote() :: not_quote() | null | string().
-type insert_options() :: #{cast => integer}.
-type insert_result() :: {ok, any} | {error, any()}.

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
    schema/0,
    comma_separated/1,
    insert/3, insert/4, insert/5, insert/6
]).

-define(schema, public).
-define(should_quote(Value), is_list(Value) orelse is_binary(Value)).

-spec equery(Query :: string(), Params :: list(), Transaction :: any()) -> any().

equery(Query, Params, Transaction) ->
    z_db:squery(format_query(Query, Params), Transaction).

-spec equery(Query :: string(), Params :: list()) -> any().

equery(Query, Params) -> equery(Query, Params, todow:context()).

-spec equery(Query :: string()) -> any().

equery(Query) -> equery(Query, []).

-spec quote(Value :: any()) -> {ok, string()} | todow_convert_utils:not_string().

quote(Value) ->
    case todow_convert_utils:to_string(Value) of
        {ok, String} -> {ok, lists:concat(["'", String, "'"])};
        Error -> Error
    end.

-spec not_quote(Value :: any()) -> not_quote().

not_quote(Value) -> {not_quote, Value}.

-spec maybe_quote(Value :: any()) -> maybe_quote().

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

-spec maybe_quote_mult(Values :: list()) -> list(maybe_quote()).

maybe_quote_mult(Values) -> [maybe_quote(Value) || Value <- Values].

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

-spec schema() -> atom().

schema() -> ?schema.

-spec comma_separated(List :: list()) -> string().

comma_separated(List) ->
    string:join(todow_convert_utils:must_to_string_mult(List), ", ").

-spec not_quoted_comma_separated(List :: list()) -> not_quote(string()).

not_quoted_comma_separated(List) -> not_quote(comma_separated(List)).

-spec columns_to_string(List :: list()) -> not_quote(string()).

columns_to_string(Columns) -> not_quoted_comma_separated(Columns).

-spec values_to_string(List :: list()) -> not_quote(string()).

values_to_string(Values) -> not_quoted_comma_separated(maybe_quote_mult(Values)).

-spec insert(
    TableName :: atom(), Columns :: list(atom()), Values :: list()
) -> {ok, pos_integer()} | {error, any()}.

insert(TableName, Columns, Values) ->
    insert(?schema, TableName, Columns, Values).

-spec insert(
    Schema :: atom(),
    TableName :: atom(),
    Columns :: list(atom()),
    Values :: list()
) -> {ok, pos_integer()} | {error, any()}.

insert(Schema, TableName, Columns, Values) ->
    insert(Schema, TableName, Columns, Values, id, #{cast => integer}).

-spec insert(
    TableName :: atom(),
    Columns :: list(atom()),
    Values :: list(),
    Returning :: atom(),
    Options :: insert_options()
) -> insert_result().

insert(TableName, Columns, Values, Returning, Options) ->
    insert(?schema, TableName, Columns, Values, Returning, Options).

-spec insert(
    Schema :: atom(),
    TableName :: atom(),
    Columns :: list(atom()),
    Values :: list(),
    Returning :: atom(),
    Options :: insert_options()
) -> insert_result().

insert(Schema, TableName, Columns, Values, Returning, Options) ->
    ColumnsAsString = columns_to_string(Columns),
    ValuesAsString = values_to_string(Values),
    Query = "INSERT INTO $1.$2 ($3) VALUES ($4) RETURNING $5",
    Args = [Schema, TableName, ColumnsAsString, ValuesAsString, Returning],
    QueryFormatted = format_query(Query, Args),
    Result = equery(QueryFormatted),
    process_insert_result(Result, Options).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

-spec do_reformat_query(Queries :: list(string()), Acc :: string()) -> string().

do_reformat_query([], Acc) ->
    Acc;
do_reformat_query([LastQuery], Acc) ->
    Acc ++ format(LastQuery) ++ ";";
do_reformat_query([Query | Queries], Acc) ->
    do_reformat_query(Queries, Acc ++ format(Query) ++ " ").

-spec process_insert_result(
    Result :: any(), Options :: insert_options()
) -> insert_result().

process_insert_result({ok, 1, _Columns, [{Value}]}, Options) ->
    {ok, process_insert_result_options(Value, Options)};
process_insert_result({error, _} = Error, _Options) ->
    Error.

-spec process_insert_result_options(
    Value :: any(), Options :: insert_options()
) -> any().

process_insert_result_options(Value, #{cast := integer}) ->
    todow_convert_utils:must_to_integer(Value);
process_insert_result_options(Value, #{}) ->
    Value.
