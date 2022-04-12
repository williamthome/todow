-module(todow_schema_sup).
-behavior(supervisor).

-define(SERVER, ?MODULE).

%% API functions
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%%%=============================================================================
%%% API functions
%%%=============================================================================
-spec start_link() -> supervisor:startlink_ret().

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%=============================================================================
%%% Supervisor callbacks
%%%=============================================================================
-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.

init([]) ->
    SupFlags = #{
        strategy => one_for_one
    },
    Children = [
        child_spec(todo_schema)
    ],
    {ok, {SupFlags, Children}}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================
-spec child_spec(Module :: module()) -> supervisor:child_spec().

child_spec(Module) ->
    #{
        id => Module,
        start => {todow_schema, start_link, [Module]}
    }.
