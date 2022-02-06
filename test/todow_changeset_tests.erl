-module(todow_changeset_tests).

-import(todow_changeset, [
  cast/2, cast/3
]).

-include_lib("eunit/include/eunit.hrl").

cast_test() ->
  Data = #{},
  Params = #{foo => bar, bar => baz},
  Permitted = [foo],
  ?assertEqual(#{foo => bar, bar => baz}, cast(Data, Params)),
  ?assertEqual(#{foo => bar}, cast(Data, Params, Permitted)).
