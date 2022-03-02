-module(todow_todos_sitetest).

-dialyzer([no_return, no_match]).

-include_lib("eunit/include/eunit.hrl").

-define(run, is_pid(whereis(zotonic_launcher_sup))).

%%--------------------------------------------------------------------
%% Setup
%%--------------------------------------------------------------------

todos_test_() ->
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
  }.

setup() ->
  Schema = todow_test,
  todow_db_schema:setup(Schema),
  #{schema => Schema}.

cleanup(#{schema := Schema}) ->
  todow_db_schema:cleanup(Schema).

success() ->
  [
    fun test_insert/1
  ].

%%--------------------------------------------------------------------
%% Tests
%%--------------------------------------------------------------------

test_insert(#{schema := Schema}) ->
  Expected = {ok, 1},
  Result = todow_db:insert(Schema, todos, [title], ["mytodo"]),
  assertEqual("Ensure insert todo", Expected, Result).

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

assertEqual(Title, Expected, Result) ->
  a(Title, ?_assertEqual(Expected, Result)).

a(Title, Assert) ->
  case ?run of
    true -> {Title, Assert};
    false -> {Title, skip_assert()}
  end.

skip_assert() -> ?_assert(true).
