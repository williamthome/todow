-module(todow_changeset_tests).

-import(todow_changeset, [
  new/2,
  cast/2, cast/3
]).

-include_lib("eunit/include/eunit.hrl").

cast_test() ->
  Changeset0 = cast(
    #{},
    #{foo => bar, bar => baz}
  ),
  ExpectedChangeset0 = new(
    #{foo => bar, bar => baz},
    #{foo => bar, bar => baz}
  ),
  ?assertEqual(ExpectedChangeset0, Changeset0),

  Changeset1 = cast(
    #{},
    #{foo => bar, bar => baz},
    [foo]
  ),
  ExpectedChangeset1 = new(
    #{foo => bar},
    #{foo => bar}
  ),
  ?assertEqual(ExpectedChangeset1, Changeset1),

  Changeset2 = cast(
    #{foo => bar},
    #{bar => baz}
  ),
  ExpectedChangeset2 = new(
    #{bar => baz},
    #{foo => bar, bar => baz}
  ),
  ?assertEqual(ExpectedChangeset2, Changeset2).
