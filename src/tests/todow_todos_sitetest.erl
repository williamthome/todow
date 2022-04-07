-module(todow_todos_sitetest).

-dialyzer([no_return, no_match]).

-include_lib("eunit/include/eunit.hrl").

%%%=============================================================================
%%% Setup
%%%=============================================================================

todos_test_() ->
  maybe_test_def(
    {"Todos Tests",
      [
        {"Insert Test",
          {foreach,
            fun setup/0,
            fun cleanup/1,
            success()
          }
        }
      ]
    }
  ).

setup() ->
  Schema = todow_test,
  todow_db_schema:recreate(Schema),
  #{schema => Schema}.

cleanup(#{schema := Schema}) ->
  todow_db_schema:drop(Schema).

success() ->
  [
    fun test_insert/1,
    fun test_update/1,
    fun test_transaction/1
  ].

%%%=============================================================================
%%% Tests
%%%=============================================================================

test_insert(Options) ->
  Expected = {ok, 1},
  Result = todow_db:insert(todos, {[title], ["foo"]}, Options),
  {"Ensure insert todo", ?_assertEqual(Expected, Result)}.

test_update(Options) ->
  {ok, Id} = todow_db:insert(todos, {[title], ["foo"]}, Options),
  Expected = {ok, Id},
  Result = todow_db:update_by_id(todos, Id, {[title], ["bar"]}, Options),
  {"Ensure update todo", ?_assertEqual(Expected, Result)}.

test_transaction(Options) ->
  Expected = {ok, 1},
  Result = todow_db:transaction(
    fun(_Conn) ->
      {ok, Id} = todow_db:insert(todos, {[title], ["foo"]}, Options),
      todow_db:update_by_id(todos, Id, {[title], ["bar"]}, Options)
    end
  ),
  {"Ensure execute transaction", ?_assertEqual(Expected, Result)}.

%%%=============================================================================
%%% Helpers
%%%=============================================================================

-define(SERVER, zotonic_launcher_sup).
-define(SITE, todow_sup).

is_running(What) ->
  is_pid(whereis(What)).

%% TODO: Start Zotonic before eunit tests and remove this flag
can_run_tests() ->
  is_running(?SERVER) andalso is_running(?SITE).

maybe_test_def(Def) ->
  case can_run_tests() of
    true -> Def;
    false -> []
  end.
