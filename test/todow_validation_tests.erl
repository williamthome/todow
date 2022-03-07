-module(todow_validation_tests).

-import(todow_validation, [
  validates_required/1, required_validation/0,
  validates_is_integer/1, is_integer_validation/0,
  validates_range/3, range_validation/2,
  validate/2
]).

-include_lib("eunit/include/eunit.hrl").

validates_required_test() ->
  ?assertMatch({error, _}, validates_required(undefined)),
  ?assertMatch({error, _}, validates_required("")),
  ?assertMatch({error, _}, validates_required(<<>>)),
  ?assertEqual({ok, foo}, validates_required(foo)).

validates_range_test() ->
  ?assertMatch({error, _}, validates_range(-1, 0, 1)),
  ?assertMatch({ok, 0}, validates_range(0, 0, 1)).

validate_test() ->
  ?assertMatch(
    {error, foo},
    validate(
      [
        fun(V) -> {ok, V} end,
        fun(V) -> {error, V} end,
        fun(V) -> {ok, V} end
      ],
      foo
    )
  ),
  ?assertMatch(
    {ok, foo},
    validate(
      [
        fun(V) -> {ok, V} end,
        fun(V) -> {ok, V} end
      ],
      foo
    )
  ),
  ?assertMatch({ok, foo}, validate([], foo)).
