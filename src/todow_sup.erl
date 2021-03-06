-module(todow_sup).
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
        #{
            id => todow_db,
            start => {todow_db, start_link, [#{adapter => zotonic_db_adapter}]}
        },
        #{
            id => todow_schema_sup,
            start => {todow_schema_sup, start_link, []}
        }
    ],
    {ok, {SupFlags, Children}}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

% nothing here!
