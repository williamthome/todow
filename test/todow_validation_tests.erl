-module(todow_validation_tests).

-import(todow_validation, [
  validate_required/1,
  validate_number/2,

  validate/1, validate/2
]).

-include_lib("eunit/include/eunit.hrl").

validate_required_test() ->
  ?assertEqual({error, required}, validate_required(undefined)),
  ?assertEqual({error, required}, validate_required("")),
  ?assertEqual({error, required}, validate_required(<<>>)),
  ?assertEqual(ok, validate_required(ok)).

validate_number_test() ->
  ?assertEqual({error, {range, {0, 1}}}, validate_number(-1, #{range => {0, 1}})),
  ?assertEqual(ok, validate_number(1, #{range => {0, 1}})).

validate_test() ->
  ?assertEqual({error, required}, validate(undefined, [validate_required])),
  ?assertEqual({error, required}, validate("", [{validate_required, #{}}])),
  ?assertEqual({error, required}, validate(<<>>, [{todow_validation, validate_required}])),
  ?assertEqual(ok, validate(ok, [{todow_validation, validate_required, #{}}])),
  ?assertEqual({error, {foo, {range, {0, 1}}}}, validate({foo, -1}, [ validate_required, {validate_number, #{range => {0, 1}}} ])),
  ?assertEqual({ok, foo}, validate({foo, 1}, [ validate_required, {validate_number, #{range => {0, 1}}} ])).

validate_multiple_test() ->
  ?assertEqual(#{errors => [{foo, required}], valid => false}, validate([{{foo, undefined}, [validate_required]}])),
  ?assertEqual(#{errors => [], valid => true}, validate([{{foo, ok}, [validate_required]}])).
