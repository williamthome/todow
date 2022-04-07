-module(todow_db).
-behaviour(gen_server).

-include("./include/todow.hrl").

-define(SERVER, ?MODULE).

-define(DEFAULT_SCHEMA, public).
-define(DEFAULT_COLUMN, id).
-define(DEFAULT_TRANSFORM, fun todow_convert_utils:maybe_to_integer/1).
-define(DEFAULT_ARGS, #{
    default_schema => ?DEFAULT_SCHEMA
}).
-define(DEFAULT_OPTIONS, #{
    schema => ?DEFAULT_SCHEMA,
    returning => #{
        column => ?DEFAULT_COLUMN,
        transform => ?DEFAULT_TRANSFORM
    }
}).

-define(should_quote(Value), is_list(Value) orelse is_binary(Value)).

-type schema() :: atom().
-type adapter() :: atom().
-type init_args() :: #{
    default_schema => schema(),
    adapter => adapter()
}.
-type id() :: pos_integer().
-type column() :: todow_db_query:column().
-type transform() :: fun((1) -> any()).
-type returning() ::
    column()
    | #{column => column(), transform => transform()}.
-type options() :: #{
    schema => schema(),
    returning => returning()
}.
-type result(Type) :: {ok, Type} | {error, any()}.
-type result() :: result(any()).
-type query_params() :: todow_db_query:params().
-type payload() :: todow_db_query:payload().
-type connection() :: any().
-type transaction_fun() :: fun((connection()) -> result()).

-record(state, {
    default_schema :: schema(),
    % -behaviour(todow_db_adapter).
    adapter :: adapter()
}).

-export_type([
    id/0,
    options/0,
    result/0,
    connection/0,
    transaction_fun/0
]).

%% API functions
-export([
    start_link/1,
    get_default_schema/0,
    fquery/2,
    equery/1, equery/2,
    fequery/2, fequery/3,
    insert/2, insert/3,
    update/4, update/5,
    update_by_id/3, update_by_id/4,
    transaction/1, transaction/2
]).

%% GenServer callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================
-spec start_link(Args :: init_args()) -> gen:start_ret().

start_link(#{default_schema := DefaultSchema, adapter := Adapter}) ->
    State = #state{
        default_schema = DefaultSchema,
        adapter = Adapter
    },
    gen_server:start_link({local, ?SERVER}, ?MODULE, State, []);
start_link(Args) ->
    start_link(maps:merge(?DEFAULT_ARGS, Args)).

%%------------------------------------------------------------------------------
%% @doc Returns the default schema.
%% @end
%%------------------------------------------------------------------------------
-spec get_default_schema() -> schema().

get_default_schema() -> gen_server:call(?SERVER, schema).

%%------------------------------------------------------------------------------
%% @doc Formats a query.
%% @end
%%------------------------------------------------------------------------------
-spec fquery(Query :: string(), Params :: query_params()) -> string().

fquery(Query, Params) -> gen_server:call(?SERVER, {fquery, Query, Params}).

%%------------------------------------------------------------------------------
%% @doc Executes a query.
%% @end
%%------------------------------------------------------------------------------
-spec equery(Query :: string()) -> any().

equery(Query) -> equery(Query, maps:new()).

%%------------------------------------------------------------------------------
%% @doc Executes a query.
%% @end
%%------------------------------------------------------------------------------
-spec equery(Query :: string(), Options :: options()) -> any().

equery(Query, Options) -> gen_server:call(?SERVER, {equery, Query, Options}).

%%------------------------------------------------------------------------------
%% @doc Formats and executes a query.
%% @end
%%------------------------------------------------------------------------------
-spec fequery(Query :: string(), Params :: query_params()) -> any().

fequery(Query, Params) -> fequery(Query, Params, maps:new()).

%%------------------------------------------------------------------------------
%% @doc Formats and executes a query.
%% @end
%%------------------------------------------------------------------------------
-spec fequery(
    Query :: string(),
    Params :: query_params(),
    Options :: options()
) -> any().

fequery(Query, Params, Options) ->
    gen_server:call(?SERVER, {fequery, Query, Params, Options}).

%%------------------------------------------------------------------------------
%% @doc Inserts data into db.
%% @end
%%------------------------------------------------------------------------------
-spec insert(Table :: atom(), Payload :: payload()) -> result().

insert(Table, Payload) -> insert(Table, Payload, ?DEFAULT_OPTIONS).

%%------------------------------------------------------------------------------
%% @doc Inserts data into db.
%% @end
%%------------------------------------------------------------------------------
-spec insert(
    Table :: atom(),
    Payload :: payload(),
    Options :: options()
) -> result().

insert(Table, Payload, Options) ->
    gen_server:call(?SERVER, {insert, Table, Payload, Options}).

%%------------------------------------------------------------------------------
%% @doc Updates db data.
%% @end
%%------------------------------------------------------------------------------
-spec update(
    Table :: atom(),
    Payload :: payload(),
    ClauseQuery :: string(),
    ClauseParams :: query_params()
) -> result().

update(Table, Payload, ClauseQuery, ClauseParams) ->
    update(Table, Payload, ClauseQuery, ClauseParams, ?DEFAULT_OPTIONS).

%%------------------------------------------------------------------------------
%% @doc Updates db data.
%% @end
%%------------------------------------------------------------------------------
-spec update(
    Table :: atom(),
    Payload :: payload(),
    ClauseQuery :: string(),
    ClauseParams :: query_params(),
    Options :: options()
) -> result().

update(Table, Payload, ClauseQuery, ClauseParams, Options) ->
    gen_server:call(
        ?SERVER,
        {update, Table, Payload, ClauseQuery, ClauseParams, Options}
    ).

%%------------------------------------------------------------------------------
%% @doc Updates db data by id.
%% @end
%%------------------------------------------------------------------------------
-spec update_by_id(
    Table :: atom(),
    Id :: id(),
    Payload :: payload()
) -> result().

update_by_id(Table, Id, Payload) ->
    update_by_id(Table, Id, Payload, ?DEFAULT_OPTIONS).

%%------------------------------------------------------------------------------
%% @doc Updates db data by id.
%% @end
%%------------------------------------------------------------------------------
-spec update_by_id(
    Table :: atom(),
    Id :: id(),
    Payload :: payload(),
    Options :: options()
) -> result().

update_by_id(Table, Id, Payload, Options) ->
    ClauseQuery = "WHERE id = $1",
    ClauseParams = [Id],
    update(Table, Payload, ClauseQuery, ClauseParams, Options).

%%------------------------------------------------------------------------------
%% @doc Executes a function in a transaction.
%% @end
%%------------------------------------------------------------------------------
-spec transaction(Fun :: transaction_fun()) -> result().

transaction(Fun) ->
    transaction(undefined, Fun).

%%------------------------------------------------------------------------------
%% @doc Executes a function in a transaction.
%% @end
%%------------------------------------------------------------------------------
-spec transaction(
    Connection :: connection(), Fun :: transaction_fun()
) -> result().

transaction(Connection, Fun) ->
    gen_server:call(
        ?SERVER,
        {transaction, Connection, Fun}
    ).

%%%=============================================================================
%%% GenServer callbacks
%%%=============================================================================
-spec init(State :: #state{}) -> {ok, #state{}}.

init(#state{} = State) -> {ok, State}.

handle_call(schema, _From, #state{default_schema = Schema} = State) ->
    {reply, Schema, State};

handle_call({fquery, Query, Params}, _From, State) ->
    Reply = do_fquery(Query, Params),
    {reply, Reply, State};

handle_call(
    {equery, Query, Options},
    _From,
    #state{adapter = Adapter} = State
) ->
    Reply = do_equery(Adapter, Query, Options),
    {reply, Reply, State};

handle_call({fequery, Query, Params, Options}, _From, #state{adapter = Adapter} = State) ->
    Reply = do_fequery(Adapter, Query, Params, Options),
    {reply, Reply, State};

handle_call({insert, Table, Payload, Options}, _From, #state{adapter = Adapter} = State) ->
    Schema = fetch_schema(Options, State),
    Reply = do_insert(Adapter, Schema, Table, Payload, Options),
    {reply, Reply, State};

handle_call(
    {update, Table, Payload, ClauseQuery, ClauseParams, Options},
    _From,
    #state{adapter = Adapter} = State
) ->
    Schema = fetch_schema(Options, State),
    Reply = do_update(Adapter, Schema, Table, Payload, ClauseQuery, ClauseParams, Options),
    {reply, Reply, State};

handle_call(
    {transaction, ConnectionOrUndefined, Fun},
    _From,
    #state{adapter = Adapter} = State
) ->
    Connection = fetch_connection(ConnectionOrUndefined, Adapter),
    Reply = Adapter:transaction(Connection, Fun),
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

do_fquery(Query, Params) ->
    todow_db_query:format_query(Query, Params).

do_equery(Adapter, Query, Options) ->
    Result = Adapter:equery(Query),
    maybe_transform(Result, Options).

do_fequery(Adapter, Query, Params, Options) ->
    QueryToExecute = do_fquery(Query, Params),
    do_equery(Adapter, QueryToExecute, Options).

do_insert(Adapter, Schema, Table, Payload, Options) ->
    Returning = fetch_column(Options),
    Query = todow_db_query:insert_query(Schema, Table, Payload, Returning),
    do_equery(Adapter, Query, Options).

do_update(Adapter, Schema, Table, Payload, ClauseQuery, ClauseParams, Options) ->
    Returning = fetch_column(Options),
    Query = todow_db_query:update_query(
        Schema, Table, Payload, ClauseQuery, ClauseParams, Returning
    ),
    do_equery(Adapter, Query, Options).

-spec fetch_schema(Options :: options(), #state{}) -> schema().

fetch_schema(#{schema := undefined}, #state{default_schema = Schema}) ->
    Schema;
fetch_schema(#{schema := Schema}, #state{}) ->
    Schema.

-spec fetch_column(Options :: options()) -> column().

fetch_column(#{returning := #{column := Column}}) -> Column;
fetch_column(#{returning := Column}) -> Column;
fetch_column(_Options) -> ?DEFAULT_COLUMN.

-spec fetch_transform(
    Options :: options()
) -> transform() | undefined.

fetch_transform(#{returning := #{transform := Transform}}) ->
    Transform;
fetch_transform(Options) ->
    case fetch_column(Options) =:= ?DEFAULT_COLUMN of
        true -> ?DEFAULT_TRANSFORM;
        false -> undefined
    end.

-spec maybe_transform(Result :: any(), Options :: options()) -> any().

maybe_transform(Result, Options) ->
    case fetch_transform(Options) of
        undefined -> Result;
        Transform -> do_transform(Transform, Result)
    end.

do_transform(_Transform, {error, _Reason} = Error) ->
    Error;
do_transform(Transform, {ok, Result}) ->
    {ok, Transform(Result)}.

fetch_connection(undefined, Adapter) ->
    Adapter:get_connection();
fetch_connection(Connection, _Adapter) ->
    Connection.
