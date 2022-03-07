-module(todow_validation).

-include("./include/todow.hrl").

-type validates_ok() :: {ok, any()}.
-type validates_error() :: {error, {atom(), any(), string()}}.
-type validates_result() :: validates_ok() | validates_error().
-type validates() :: fun((1) -> validates_result()).
-type validation() :: fun(() -> validates()).

-export_type([
  validates_ok/0,
  validates_error/0,
  validates_result/0,
  validates/0,
  validation/0
]).

-export([
  validates_required/1,
  validates_is_integer/1,
  validates_range/3
]).
-export([
  required_validation/0,
  is_integer_validation/0,
  range_validation/2
]).
-export([ validate/2 ]).

-define(is_defined(Value),
  Value =/= undefined andalso
  Value =/= "" andalso
  Value =/= <<>>
).

-define(is_not_empty(Value),
  ?is_defined(Value) andalso
  Value =/= []
).

-define(in_range(Value, Min, Max),
  is_number(Value) andalso
  Value >= Min andalso
  Value =< Max
).

%%------------------------------------------------------------------------------
%% @doc Validates if the value is defined.
%% @end
%%------------------------------------------------------------------------------

-spec validates_required(Value :: any()) -> validates_result().

validates_required(Value) when ?is_defined(Value) ->
  do_ok(Value);

validates_required(Value) ->
  Msg = "Value is required",
  do_error(required, Value, Msg).

%%------------------------------------------------------------------------------
%% @doc Validates required constructor.
%% @end
%%------------------------------------------------------------------------------

-spec required_validation() -> validates().

required_validation() ->
  fun validates_required/1.

%%------------------------------------------------------------------------------
%% @doc Validates if the value is of integer type.
%% @end
%%------------------------------------------------------------------------------

-spec validates_is_integer(Value :: any()) -> validates_result().

validates_is_integer(Value) when is_integer(Value) ->
  do_ok(Value);

validates_is_integer(Value) ->
  Msg = "Value must be an integer.",
  do_error(is_integer, Value, Msg).

%%------------------------------------------------------------------------------
%% @doc Validates is integer constructor.
%% @end
%%------------------------------------------------------------------------------

-spec is_integer_validation() -> validates().

is_integer_validation() ->
  fun validates_is_integer/1.

%%------------------------------------------------------------------------------
%% @doc Validates number range.
%% @end
%%------------------------------------------------------------------------------

-spec validates_range(
  Value :: any(), Min :: integer(), Max :: integer()
) -> validates_result().

validates_range(Value, Min, Max) when ?in_range(Value, Min, Max) ->
  do_ok(Value);

validates_range(Value, Min, Max) ->
  Msg = lists:concat(["Value must a number between ", Min, " and ", Max, "."]),
  do_error(range, Value, Msg).

%%------------------------------------------------------------------------------
%% @doc Validates range constructor.
%% @end
%%------------------------------------------------------------------------------

-spec range_validation(Min :: integer(), Max :: integer()) -> validates().

range_validation(Min, Max) ->
  fun(Value) -> validates_range(Value, Min, Max) end.

%%------------------------------------------------------------------------------
%% @doc Validate by recursion. Halts on the first error.
%% @end
%%------------------------------------------------------------------------------

-spec validate(
  Validations :: list(validation()), Value :: any()
) -> validates_result().

validate(Validations, Value) ->
  do_validate(Validations, Value, do_ok(Value)).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Process the validate/2 result.
%% @end
%%------------------------------------------------------------------------------

-spec do_validate(
  Validations :: list(validation()),
  Value :: any(),
  Result :: validates_result()
) -> validates_result().

do_validate([], _Value, Result) -> Result;

do_validate(_Validations, _Value, {error, _} = Error) -> Error;

do_validate([Validates | Validations], Value, _Result) ->
  Result = Validates(Value),
  do_validate(Validations, Value, Result).

%%------------------------------------------------------------------------------
%% @doc Validates ok result constructor.
%% @end
%%------------------------------------------------------------------------------

-spec do_ok(any()) -> validates_ok().

do_ok(Value) -> ?OK(Value).

%%------------------------------------------------------------------------------
%% @doc Validates error result constructor.
%% @end
%%------------------------------------------------------------------------------

-spec do_error(atom(), any(), string()) -> validates_error().

do_error(Reason, Value, Msg) -> ?ERROR({Reason, Value, Msg}).
