-module(todow_sup).
-behavior(supervisor).

%% API functions
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
-define(DB_SPEC(Id, Args), #{
    id => Id,
    start => {todow_db, start_link, [Args]}
}).

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
        ?DB_SPEC(todow_db, #{adapter => zotonic_db_adapter})
    ],
    {ok, {SupFlags, Children}}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

% nothing here!
