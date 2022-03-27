% TODO: Change to gen_server behavior
-module(todow_db).

-include("./include/todow.hrl").

-define(SCHEMA, public).
-define(CONTEXT, todow:context()).

-define(should_quote(Value), is_list(Value) orelse is_binary(Value)).

-type id() :: pos_integer().
-type options() :: #{cast => integer}.
-type result(Type) :: {ok, Type} | {error, any()}.
-type result() :: result(any()).
-type result_id() :: result(id()).

-export([
    equery/1, equery/2, equery/3,
    schema/0,
    insert/2, insert/3, insert/4, insert/5,
    update/7,
    update_by_id/4
]).

%%------------------------------------------------------------------------------
%% @doc Executes a query.
%% @end
%%------------------------------------------------------------------------------
-spec equery(Query :: todow_db_query:t()) -> any().

equery({Query, Params}) -> equery(Query, Params);
equery(Query) -> equery(Query, []).

%%------------------------------------------------------------------------------
%% @doc Executes a query.
%% @end
%%------------------------------------------------------------------------------
-spec equery(Query :: string(), Params :: todow_db_query:params()) -> any().

% TODO: Remove equery/2 because context must come from a db adapter
equery(Query, Params) -> equery(Query, Params, ?CONTEXT).

%%------------------------------------------------------------------------------
%% @doc Executes a query.
%% @end
%%------------------------------------------------------------------------------
-spec equery(
    Query :: string(), Params :: todow_db_query:params(), Transaction :: any()
) -> any().

equery(Query, Params, Transaction) ->
    % TODO: Change module to be a gen_server and set this as an adapter
    z_db:squery(
        todow_db_query:format_query(Query, Params),
        Transaction
    ).

%%------------------------------------------------------------------------------
%% @doc Returns the db schema.
%% @end
%%------------------------------------------------------------------------------
-spec schema() -> ?SCHEMA.

% TODO: Schema must be defined in state when db will be a gen_server
schema() -> ?SCHEMA.

%%------------------------------------------------------------------------------
%% @doc Inserts data into db.
%% @end
%%------------------------------------------------------------------------------
-spec insert(
    Table :: atom(),
    Payload :: todow_db_query:payload()
) -> result_id().

insert(Table, Payload) -> insert(?SCHEMA, Table, Payload).

%%------------------------------------------------------------------------------
%% @doc Inserts data into db.
%% @end
%%------------------------------------------------------------------------------
-spec insert(
    Schema :: atom(), Table :: atom(), Payload :: todow_db_query:payload()
) -> result_id().

insert(Schema, Table, Payload) ->
    insert(Schema, Table, Payload, id, #{cast => integer}).

%%------------------------------------------------------------------------------
%% @doc Inserts data into db.
%% @end
%%------------------------------------------------------------------------------
-spec insert(
    Table :: atom(),
    Payload :: todow_db_query:payload(),
    Returning :: todow_db_query:column(),
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
    Payload :: todow_db_query:payload(),
    Returning :: todow_db_query:column(),
    Options :: options()
) -> result().

insert(Schema, Table, Payload, Returning, Options) ->
    Query = todow_db_query:insert_query(Schema, Table, Payload, Returning),
    Result = equery(Query),
    process_result(Result, Options).

%%------------------------------------------------------------------------------
%% @doc Updates db data.
%% @end
%%------------------------------------------------------------------------------
-spec update(
    Schema :: atom(),
    Table :: atom(),
    Payload :: todow_db_query:payload(),
    ClauseQuery :: string(),
    ClauseParams :: todow_db_query:params(),
    Returning :: todow_db_query:column(),
    Options :: options()
) -> result().

update(Schema, Table, Payload, ClauseQuery, ClauseParams, Returning, Options) ->
    Query = todow_db_query:update_query(
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
    Payload :: todow_db_query:payload()
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
