-module(todow_changeset_tests).

-import(todow_changeset, [
    new/3,
    cast/3, cast/4
]).

-include_lib("eunit/include/eunit.hrl").

cast_test_() ->
    [
        {
            "Empty data returns full changes",
            ?_assertEqual(
                new(#{foo => bar, bar => baz}, #{foo => bar, bar => baz}, new),
                cast(#{}, #{foo => bar, bar => baz}, [foo, bar])
            )
        },
        {
            "Empty data returns only valid changes",
            ?_assertEqual(
                new(#{foo => bar}, #{foo => bar}, new),
                cast(#{}, #{foo => bar, bar => baz}, [foo])
            )
        },
        {
            "Merge data and changes returns full value",
            ?_assertEqual(
                new(#{foo => bar, bar => baz}, #{bar => baz}, update),
                cast(#{foo => bar}, #{bar => baz}, [foo, bar])
            )
        },
        {
            "Merge data and changes returns only valid values",
            ?_assertEqual(
                new(#{foo => bar}, #{}, update),
                cast(#{foo => bar}, #{bar => baz}, [foo])
            )
        }
    ].
