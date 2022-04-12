% TODO: Bring todow_schema_core code to this module and deletes it
-module(todow_schema).
-behavior(gen_server).

-define(SERVER, ?MODULE).

-record(state, {
    module :: module(),
    schema :: todow_schema_core:t()
}).

-callback schema() -> todow_schema_core:t().
-callback cast(Changes :: map()) -> {ok, todow_changeset:t()}.
-callback validates(
    Changeset :: todow_changeset:t()
) -> todow_schema_core:validates().

-optional_callbacks([
    cast/1,
    validates/1
]).

%% API functions
-export([
    start_link/1,
    cast/2,
    validates/2
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
-spec start_link(Module :: module()) -> gen:start_ret().

start_link(Module) ->
    State = #state{
        module = Module,
        schema = Module:schema()
    },
    gen_server:start_link({local, Module}, ?SERVER, State, []).

cast(Module, Changes) ->
    gen_server:call(Module, {cast, Changes}).

validates(Module, Changeset) ->
    gen_server:call(Module, {validates, Changeset}).

%%%=============================================================================
%%% GenServer callbacks
%%%=============================================================================
-spec init(State :: #state{}) -> {ok, #state{}}.

init(#state{} = State) -> {ok, State}.

handle_call({cast, Changes}, _From, #state{schema = Schema} = State) ->
    Reply = todow_schema_core:cast(Schema, Changes),
    {reply, Reply, State};
handle_call({validates, Changeset}, _From, #state{schema = Schema} = State) ->
    Reply = todow_schema_core:validates(Schema, Changeset),
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
