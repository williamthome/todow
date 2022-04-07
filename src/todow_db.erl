-module(todow_db).
-behaviour(gen_server).

-include("./include/todow.hrl").
-include("./include/todow_db.hrl").

-define(SERVER, ?MODULE).
-define(DEFAULT_ARGS, #{
    default_schema => ?DEFAULT_SCHEMA
}).

-type init_args() :: #{
    default_schema => schema(),
    adapter => adapter()
}.

-record(state, {
    default_schema :: schema(),
    % -behaviour(todow_db_adapter).
    adapter :: adapter()
}).

%% API functions
% TODO: Rename equery to execute
-export([
    start_link/1,
    equery/1, equery/2, equery/3,
    fequery/2, fequery/3, fequery/4,
    insert/2, insert/3, insert/4,
    update/4, update/5, update/6,
    update_by_id/3, update_by_id/4, update_by_id/5,
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
%% @doc Executes a query.
%% @end
%%------------------------------------------------------------------------------
-spec equery(Query :: query()) -> result().

equery(Query) -> equery(Query, maps:new()).

%%------------------------------------------------------------------------------
%% @doc Executes a query.
%% @end
%%------------------------------------------------------------------------------
-spec equery(Query :: query(), Options :: options()) -> result().

equery(Query, Options) -> equery(undefined, Query, Options).

%%------------------------------------------------------------------------------
%% @doc Executes a query.
%% @end
%%------------------------------------------------------------------------------
-spec equery(
    Connection :: connection(),
    Query :: query(),
    Options :: options()
) -> result().

equery(Connection, Query, Options) ->
    gen_server:call(?SERVER, {equery, Connection, Query, Options}).

%%------------------------------------------------------------------------------
%% @doc Formats and executes a query.
%% @end
%%------------------------------------------------------------------------------
-spec fequery(Query :: query(), Params :: query_params()) -> result().

fequery(Query, Params) -> fequery(Query, Params, maps:new()).

%%------------------------------------------------------------------------------
%% @doc Formats and executes a query.
%% @end
%%------------------------------------------------------------------------------
-spec fequery(
    Query :: query(),
    Params :: query_params(),
    Options :: options()
) -> result().

fequery(Query, Params, Options) -> fequery(undefined, Query, Params, Options).

%%------------------------------------------------------------------------------
%% @doc Formats and executes a query.
%% @end
%%------------------------------------------------------------------------------
-spec fequery(
    Connection :: connection(),
    Query :: query(),
    Params :: query_params(),
    Options :: options()
) -> result().

fequery(Connection, Query, Params, Options) ->
    gen_server:call(?SERVER, {fequery, Connection, Query, Params, Options}).

%%------------------------------------------------------------------------------
%% @doc Inserts data into db.
%% @end
%%------------------------------------------------------------------------------
-spec insert(Table :: table(), Payload :: payload()) -> result().

insert(Table, Payload) -> insert(Table, Payload, ?DEFAULT_OPTIONS).

%%------------------------------------------------------------------------------
%% @doc Inserts data into db.
%% @end
%%------------------------------------------------------------------------------
-spec insert(
    Table :: table(),
    Payload :: payload(),
    Options :: options()
) -> result().

insert(Table, Payload, Options) -> insert(undefined, Table, Payload, Options).

%%------------------------------------------------------------------------------
%% @doc Inserts data into db.
%% @end
%%------------------------------------------------------------------------------
-spec insert(
    Connection :: connection(),
    Table :: table(),
    Payload :: payload(),
    Options :: options()
) -> result().

insert(Connection, Table, Payload, Options) ->
    gen_server:call(?SERVER, {insert, Connection, Table, Payload, Options}).

%%------------------------------------------------------------------------------
%% @doc Updates db data.
%% @end
%%------------------------------------------------------------------------------
-spec update(
    Table :: table(),
    Payload :: payload(),
    ClauseQuery :: query(),
    ClauseParams :: query_params()
) -> result().

update(Table, Payload, ClauseQuery, ClauseParams) ->
    update(Table, Payload, ClauseQuery, ClauseParams, ?DEFAULT_OPTIONS).

%%------------------------------------------------------------------------------
%% @doc Updates db data.
%% @end
%%------------------------------------------------------------------------------
-spec update(
    Table :: table(),
    Payload :: payload(),
    ClauseQuery :: query(),
    ClauseParams :: query_params(),
    Options :: options()
) -> result().

update(Table, Payload, ClauseQuery, ClauseParams, Options) ->
    update(undefined, Table, Payload, ClauseQuery, ClauseParams, Options).

%%------------------------------------------------------------------------------
%% @doc Updates db data.
%% @end
%%------------------------------------------------------------------------------
-spec update(
    Connection :: connection(),
    Table :: table(),
    Payload :: payload(),
    ClauseQuery :: query(),
    ClauseParams :: query_params(),
    Options :: options()
) -> result().

update(Connection, Table, Payload, ClauseQuery, ClauseParams, Options) ->
    gen_server:call(
        ?SERVER,
        {update, Connection, Table, Payload, ClauseQuery, ClauseParams, Options}
    ).

%%------------------------------------------------------------------------------
%% @doc Updates db data by id.
%% @end
%%------------------------------------------------------------------------------
-spec update_by_id(
    Table :: table(),
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
    Table :: table(),
    Id :: id(),
    Payload :: payload(),
    Options :: options()
) -> result().

update_by_id(Table, Id, Payload, Options) ->
    update_by_id(undefined, Table, Id, Payload, Options).

%%------------------------------------------------------------------------------
%% @doc Updates db data by id.
%% @end
%%------------------------------------------------------------------------------
-spec update_by_id(
    Connection :: connection(),
    Table :: table(),
    Id :: id(),
    Payload :: payload(),
    Options :: options()
) -> result().

update_by_id(Connection, Table, Id, Payload, Options) ->
    gen_server:call(
        ?SERVER,
        {update_by_id, Connection, Table, Id, Payload, Options}
    ).

%%------------------------------------------------------------------------------
%% @doc Executes a function in a transaction.
%% @end
%%------------------------------------------------------------------------------
-spec transaction(Fun :: transaction_fun()) -> result().

transaction(Fun) -> transaction(undefined, Fun).

%%------------------------------------------------------------------------------
%% @doc Executes a function in a transaction.
%% @end
%%------------------------------------------------------------------------------
-spec transaction(
    Connection :: connection(), Fun :: transaction_fun()
) -> result().

transaction(Connection, Fun) ->
    gen_server:call(?SERVER, {transaction, Connection, Fun}).

%%%=============================================================================
%%% GenServer callbacks
%%%=============================================================================
-spec init(State :: #state{}) -> {ok, #state{}}.

init(#state{} = State) -> {ok, State}.

handle_call(
    {Function = equery, Connection, Query, Options},
    From,
    #state{adapter = Adapter} = State
) ->
    Args = [Connection, Adapter, Query, Options],
    spawn_reply(From, Function, Args),
    {noreply, State};
handle_call(
    {fequery, Connection, Query, Params, Options},
    From,
    #state{adapter = Adapter} = State
) ->
    Function = equery,
    QueryToExecute = todow_db_query:format(Query, Params),
    Args = [Connection, Adapter, QueryToExecute, Options],
    spawn_reply(From, Function, Args),
    {noreply, State};
handle_call(
    {Function = insert, Connection, Table, Payload, Options},
    From,
    #state{adapter = Adapter} = State
) ->
    Schema = fetch_schema(Options, State),
    Args = [Connection, Adapter, Schema, Table, Payload, Options],
    spawn_reply(From, Function, Args),
    {noreply, State};
handle_call(
    {Function = update, Connection, Table, Payload, ClauseQuery, ClauseParams, Options},
    From,
    #state{adapter = Adapter} = State
) ->
    Schema = fetch_schema(Options, State),
    Args = [Connection, Adapter, Schema, Table, Payload, ClauseQuery, ClauseParams, Options],
    spawn_reply(From, Function, Args),
    {noreply, State};
handle_call(
    {Function = update_by_id, Connection, Table, Id, Payload, Options},
    From,
    #state{adapter = Adapter} = State
) ->
    Schema = fetch_schema(Options, State),
    Args = [Connection, Adapter, Schema, Table, Id, Payload, Options],
    spawn_reply(From, Function, Args),
    {noreply, State};
handle_call(
    {Function = transaction, Connection, Fun},
    From,
    #state{adapter = Adapter} = State
) ->
    Args = [Connection, Adapter, Fun],
    spawn_reply(From, Function, Args),
    {noreply, State}.

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

-spec fetch_schema(Options :: options(), #state{}) -> schema().

fetch_schema(#{schema := undefined}, #state{default_schema = Schema}) ->
    Schema;
fetch_schema(#{schema := Schema}, #state{}) ->
    Schema.

-spec spawn_reply(From :: pid(), MFA :: mfa()) -> result().

spawn_reply(From, {Module, Fun, Args}) ->
    spawn(fun() ->
        Reply = apply(Module, Fun, Args),
        gen_server:reply(From, Reply)
    end).

-spec spawn_reply(From :: pid(), Fun :: atom(), Args :: list()) -> result().

spawn_reply(From, Fun, Args) ->
    spawn_reply(From, {todow_db_repo, Fun, Args}).
