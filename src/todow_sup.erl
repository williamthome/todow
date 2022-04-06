-module(todow_sup).
-behaviour(supervisor).

%% API functions
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%=============================================================================
%%% API functions
%%%=============================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%=============================================================================
%%% Supervisor callbacks
%%%=============================================================================

init([]) ->
    SupFlags = #{
        strategy => one_for_one
    },
    Children = [
        #{
            id => todow_db,
            start =>
                {todow_db, start_link, [
                    #{
                        adapter => zotonic_db_adapter
                    }
                ]}
        }
    ],
    {ok, {SupFlags, Children}}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

% nothing here!
