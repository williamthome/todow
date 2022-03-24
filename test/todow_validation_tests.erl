-module(todow_validation_tests).

-import(todow_validation, [
    validates_required/1,
    validates_is_integer/1,
    validates_is_float/1,
    validates_is_number/1,
    validates_is_binary/1,
    validates_is_boolean/1,
    validates_is_date/1,
    validates_is_time/1,
    validates_is_datetime/1,
    validates_range/3,
    validate/2
]).

-include_lib("eunit/include/eunit.hrl").

validates_required_test() ->
    ?assertMatch({error, _}, validates_required(undefined)),
    ?assertMatch({error, _}, validates_required("")),
    ?assertMatch({error, _}, validates_required(<<>>)),
    ?assertEqual({ok, foo}, validates_required(foo)).

validates_is_integer_test() ->
    ?assertMatch({error, _}, validates_is_integer(undefined)),
    ?assertMatch({ok, 0}, validates_is_integer(0)).

validates_is_float_test() ->
    ?assertMatch({error, _}, validates_is_float(0)),
    ?assertMatch({ok, 0.0}, validates_is_float(0.0)).

validates_is_number_test() ->
    ?assertMatch({error, _}, validates_is_number(undefined)),
    ?assertMatch({ok, 0}, validates_is_number(0)),
    ?assertMatch({ok, 0.0}, validates_is_number(0.0)).

validates_is_binary_test() ->
    ?assertMatch({error, _}, validates_is_binary(undefined)),
    ?assertMatch({ok, <<>>}, validates_is_binary(<<>>)).

validates_is_boolean_test() ->
    ?assertMatch({error, _}, validates_is_boolean(undefined)),
    ?assertMatch({ok, true}, validates_is_boolean(true)),
    ?assertMatch({ok, false}, validates_is_boolean(false)).

validates_is_date_test() ->
    ?assertMatch({error, _}, validates_is_date(undefined)),
    ?assertMatch({ok, {2022, 03, 23}}, validates_is_date({2022, 03, 23})),
    ?assertMatch({error, _}, validates_is_date({2022, 00, 32})).

validates_is_time_test() ->
    ?assertMatch({error, _}, validates_is_time(undefined)),
    ?assertMatch({ok, {00, 00, 00}}, validates_is_time({00, 00, 00})),
    ?assertMatch({error, _}, validates_is_time({00, 25, 61})).

validates_is_datetime_test() ->
    ?assertMatch({error, _}, validates_is_datetime(undefined)),
    ?assertMatch(
        {ok, {{2022, 03, 23}, {00, 00, 00}}},
        validates_is_datetime({{2022, 03, 23}, {00, 00, 00}})
    ).

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
