-module(todow_db_repo).

-type table() :: atom().
-type schema() :: atom().
-type column() :: atom().
-type adapter() :: atom().
-type id() :: pos_integer().
-type transform() :: fun((1) -> any()).
-type returning() ::
    column()
    | #{column => column(), transform => transform()}.
-type options() :: #{
    schema => schema(),
    returning => returning()
}.
-type value() :: any().
-type columns() :: list(column()).
-type values() :: list(value()).
-type columns_and_values_tuple() :: {columns(), values()}.
-type columns_and_values_proplist() :: list({column(), value()}).
-type payload() ::
    map()
    | columns_and_values_tuple()
    | columns_and_values_proplist().
-type connection() :: any().
-type transaction_fun() :: fun((connection()) -> todow:result()).

-export_type([
    table/0,
    schema/0,
    column/0,
    adapter/0,
    id/0,
    transform/0,
    returning/0,
    options/0,
    value/0,
    columns/0,
    values/0,
    columns_and_values_tuple/0,
    columns_and_values_proplist/0,
    payload/0,
    connection/0,
    transaction_fun/0
]).

-export([
    default_schema/0,
    default_column/0,
    default_transform/0,
    default_options/0
]).
-export([
    transform_id/0
]).
-export([
    equery/4,
    insert/6,
    update/8,
    update_by_id/7,
    transaction/3
]).

%%------------------------------------------------------------------------------
%% Defaults
%%------------------------------------------------------------------------------

-spec default_schema() -> schema().

default_schema() -> public.

-spec default_column() -> column().

default_column() -> id.

-spec default_transform() -> transform().

default_transform() -> transform_id().

-spec default_options() -> options().

default_options() ->
    #{
        schema => default_schema(),
        returning => #{
            column => default_column(),
            transform => default_transform()
        }
    }.

%%------------------------------------------------------------------------------
%% Transforms
%%------------------------------------------------------------------------------

-spec transform_id() -> transform().

transform_id() ->
    fun
        (Value) when is_integer(Value) ->
            Value;
        (undefined) ->
            undefined;
        ([]) ->
            [];
        (Value) when is_list(Value) orelse is_binary(Value) ->
            todow_convert_utils:must_to_integer(Value)
    end.

%%------------------------------------------------------------------------------
%% @doc Executes a query.
%% @end
%%------------------------------------------------------------------------------
-spec equery(
    Connection :: connection(),
    Adapter :: adapter(),
    Query :: todow_db_query:query(),
    Options :: options()
) -> todow:result().

equery(undefined, Adapter, Query, Options) ->
    Connection = Adapter:get_connection(),
    equery(Connection, Adapter, Query, Options);
equery(Connection, Adapter, Query, Options) ->
    Result = Adapter:equery(Connection, Query),
    maybe_transform(Result, Options).

%%------------------------------------------------------------------------------
%% @doc Inserts data into db.
%% @end
%%------------------------------------------------------------------------------
-spec insert(
    Connection :: connection(),
    Adapter :: adapter(),
    Schema :: schema(),
    Table :: table(),
    Payload :: payload(),
    Options :: options()
) -> todow:result().

insert(Connection, Adapter, Schema, Table, Payload, Options) ->
    Returning = fetch_column(Options),
    Query = todow_db_query:insert_query(Schema, Table, Payload, Returning),
    equery(Connection, Adapter, Query, Options).

%%------------------------------------------------------------------------------
%% @doc Updates db data.
%% @end
%%------------------------------------------------------------------------------
-spec update(
    Connection :: connection(),
    Adapter :: adapter(),
    Schema :: schema(),
    Table :: table(),
    Payload :: payload(),
    ClauseQuery :: todow_db_query:query(),
    ClauseParams :: todow_db_query:query_params(),
    Options :: options()
) -> todow:result().

update(Connection, Adapter, Schema, Table, Payload, ClauseQuery, ClauseParams, Options) ->
    Returning = fetch_column(Options),
    Query = todow_db_query:update_query(
        Schema, Table, Payload, ClauseQuery, ClauseParams, Returning
    ),
    equery(Connection, Adapter, Query, Options).

%%------------------------------------------------------------------------------
%% @doc Updates db data by id.
%% @end
%%------------------------------------------------------------------------------
-spec update_by_id(
    Connection :: connection(),
    Adapter :: adapter(),
    Schema :: schema(),
    Table :: table(),
    Id :: id(),
    Payload :: payload(),
    Options :: options()
) -> todow:result().

update_by_id(Connection, Adapter, Schema, Table, Id, Payload, Options) ->
    ClauseQuery = "WHERE id = $1",
    ClauseParams = [Id],
    update(Connection, Adapter, Schema, Table, Payload, ClauseQuery, ClauseParams, Options).

%%------------------------------------------------------------------------------
%% @doc Executes a function in a transaction.
%% @end
%%------------------------------------------------------------------------------
-spec transaction(
    Connection :: connection(),
    Adapter :: adapter(),
    Fun :: transaction_fun()
) -> todow:result().

transaction(undefined, Adapter, Fun) ->
    Connection = Adapter:get_connection(),
    transaction(Connection, Adapter, Fun);
transaction(Connection, Adapter, Fun) ->
    Adapter:transaction(Connection, Fun).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

-spec fetch_column(Options :: options()) -> column().

fetch_column(#{returning := #{column := Column}}) -> Column;
fetch_column(#{returning := Column}) -> Column;
fetch_column(_Options) -> default_column().

-spec fetch_transform(Options :: options()) -> transform() | undefined.

fetch_transform(#{returning := #{transform := Transform}}) ->
    Transform;
fetch_transform(Options) ->
    case fetch_column(Options) =:= default_column() of
        true -> default_transform();
        false -> undefined
    end.

-spec maybe_transform(
    Result :: todow:result(),
    Options :: options()
) -> todow:result().

maybe_transform(Result, Options) ->
    case fetch_transform(Options) of
        undefined -> Result;
        Transform -> do_transform(Transform, Result)
    end.

-spec do_transform(
    Transform :: transform(),
    Result :: todow:result()
) -> todow:result().

do_transform(_Transform, {error, _Reason} = Error) ->
    Error;
do_transform(Transform, {ok, Result}) ->
    {ok, Transform(Result)}.
