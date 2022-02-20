-module(todow_changeset_tests).

-import(todow_changeset, [
  new/3,
  cast/3, cast/4
]).

-include_lib("eunit/include/eunit.hrl").

cast_test() ->
  Changeset0 = cast(
    #{},
    #{foo => bar, bar => baz},
    [foo, bar]
  ),
  ExpectedChangeset0 = new(
    #{foo => bar, bar => baz},
    #{foo => bar, bar => baz},
    new
  ),
  ?assertEqual(ExpectedChangeset0, Changeset0),

  Changeset1 = cast(
    #{},
    #{foo => bar, bar => baz},
    [foo]
  ),
  ExpectedChangeset1 = new(
    #{foo => bar},
    #{foo => bar},
    new
  ),
  ?assertEqual(ExpectedChangeset1, Changeset1),

  Changeset2 = cast(
    #{foo => bar},
    #{bar => baz},
    [foo, bar]
  ),
  ExpectedChangeset2 = new(
    #{foo => bar, bar => baz},
    #{bar => baz},
    update
  ),
  ?assertEqual(ExpectedChangeset2, Changeset2).
