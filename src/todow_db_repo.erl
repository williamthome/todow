-module(todow_db_repo).

-include("./include/todow.hrl").
-include("./include/todow_db.hrl").

-export([
    equery/4,
    insert/6,
    update/8,
    update_by_id/7,
    transaction/3
]).

%%------------------------------------------------------------------------------
%% @doc Executes a query.
%% @end
%%------------------------------------------------------------------------------
-spec equery(
    Connection :: connection(), Adapter :: adapter(), Query :: query(), Options :: options()
) -> result().

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
) -> result().

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
    ClauseQuery :: query(),
    ClauseParams :: query_params(),
    Options :: options()
) -> result().

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
) -> result().

update_by_id(Connection, Adapter, Schema, Table, Id, Payload, Options) ->
    ClauseQuery = "WHERE id = $1",
    ClauseParams = [Id],
    update(Connection, Adapter, Schema, Table, Payload, ClauseQuery, ClauseParams, Options).

%%------------------------------------------------------------------------------
%% @doc Executes a function in a transaction.
%% @end
%%------------------------------------------------------------------------------
-spec transaction(
    Connection :: connection(), Adapter :: adapter(), Fun :: transaction_fun()
) -> result().

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
fetch_column(_Options) -> ?DEFAULT_COLUMN.

-spec fetch_transform(Options :: options()) -> transform() | undefined.

fetch_transform(#{returning := #{transform := Transform}}) ->
    Transform;
fetch_transform(Options) ->
    case fetch_column(Options) =:= ?DEFAULT_COLUMN of
        true -> ?DEFAULT_TRANSFORM;
        false -> undefined
    end.

-spec maybe_transform(Result :: result(), Options :: options()) -> result().

maybe_transform(Result, Options) ->
    case fetch_transform(Options) of
        undefined -> Result;
        Transform -> do_transform(Transform, Result)
    end.

-spec do_transform(Transform :: transform(), Result :: result()) -> result().

do_transform(_Transform, {error, _Reason} = Error) ->
    Error;
do_transform(Transform, {ok, Result}) ->
    {ok, Transform(Result)}.
