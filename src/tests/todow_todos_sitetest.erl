-module(todow_todos_sitetest).

-include_lib("eunit/include/eunit.hrl").

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
  {
    "Ensure insert todo",
    ?_assertEqual(Expected, Result)
  }.
