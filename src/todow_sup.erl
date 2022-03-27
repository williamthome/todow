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
        % TODO: Start db gen_server
        % #{
        %     id => todow_db,
        %     start => {todow_db, start_link, []},
        %     restart => permanent,
        %     type => worker
        % }
    ],
    {ok, {SupFlags, Children}}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

% nothing here!
