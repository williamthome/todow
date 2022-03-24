-module(todow_validation).

-include("./include/todow.hrl").

-define(is_defined(Value),
    Value =/= undefined andalso Value =/= "" andalso Value =/= <<>>
).

-define(is_not_empty(Value),
    ?is_defined(Value) andalso Value =/= []
).

-define(in_range(Value, Min, Max),
    is_number(Value) andalso Value >= Min andalso Value =< Max
).

-define(is_year(Y), is_integer(Y)).
-define(is_month(M), is_integer(M) andalso M >= 1 andalso M =< 12).
-define(is_day(D), is_integer(D) andalso D >= 1 andalso D =< 31).
-define(is_date(Date),
    is_tuple(Date) andalso
    tuple_size(Date) =:= 3 andalso
    ?is_year(element(1, Date)) andalso
    ?is_month(element(2, Date)) andalso
    ?is_day(element(3, Date))
).

-define(is_hour(H), is_integer(H) andalso H >= 0 andalso H < 24).
-define(is_minute(M), is_integer(M) andalso M >= 0 andalso M < 60).
-define(is_second(S), is_integer(S) andalso S >= 0 andalso S < 60).
-define(is_time(Time),
    is_tuple(Time) andalso
    tuple_size(Time) =:= 3 andalso
    ?is_hour(element(1, Time)) andalso
    ?is_minute(element(2, Time)) andalso
    ?is_second(element(3, Time))
).

-define(is_datetime(DateTime),
    is_tuple(DateTime) andalso
    tuple_size(DateTime) =:= 2 andalso
    ?is_date(element(1, DateTime)) andalso
    ?is_time(element(2, DateTime))
).

-type validates_ok() :: {ok, any()}.
-type validates_error() :: {error, {atom(), any(), string()}}.
-type validates_result() :: validates_ok() | validates_error().
-type validates() :: fun((1) -> validates_result()).
-type validations() :: list(validates()).

-export_type([
    validates_ok/0,
    validates_error/0,
    validates_result/0,
    validates/0,
    validations/0
]).

-export([
    is_defined/1,
    is_not_empty/1,
    in_range/3
]).
-export([
    validates_required/1,
    validates_is_integer/1,
    validates_is_float/1,
    validates_is_number/1,
    validates_is_binary/1,
    validates_is_boolean/1,
    validates_is_date/1,
    validates_is_time/1,
    validates_is_datetime/1,
    validates_range/3
]).
-export([
    required_validation/0,
    is_integer_validation/0,
    is_float_validation/0,
    is_number_validation/0,
    is_binary_validation/0,
    is_boolean_validation/0,
    is_date_validation/0,
    is_time_validation/0,
    is_datetime_validation/0,
    range_validation/2
]).
-export([validate/2]).

%%------------------------------------------------------------------------------
%% @doc Check if value is defined.
%% @end
%%------------------------------------------------------------------------------

-spec is_defined(Value :: any()) -> boolean().

is_defined(Value) -> ?is_defined(Value).

%%------------------------------------------------------------------------------
%% @doc Check if value is not empty.
%% @end
%%------------------------------------------------------------------------------

-spec is_not_empty(Value :: any()) -> boolean().

is_not_empty(Value) -> ?is_not_empty(Value).

%%------------------------------------------------------------------------------
%% @doc Check if value is not empty.
%% @end
%%------------------------------------------------------------------------------

-spec in_range(Value :: any(), Min :: number(), Max :: number()) -> boolean().

in_range(Value, Min, Max) -> ?in_range(Value, Min, Max).

%%------------------------------------------------------------------------------
%% @doc Validates if the value is defined.
%% @end
%%------------------------------------------------------------------------------

-spec validates_required(Value :: any()) -> validates_result().

validates_required(Value) when ?is_defined(Value) ->
    ?OK(Value);
validates_required(Value) ->
    Msg = "Value is required",
    ?ERROR({required, Value, Msg}).

%%------------------------------------------------------------------------------
%% @doc Validates required constructor.
%% @end
%%------------------------------------------------------------------------------

-spec required_validation() -> validates().

required_validation() -> fun validates_required/1.

%%------------------------------------------------------------------------------
%% @doc Validates if the value is of integer type.
%% @end
%%------------------------------------------------------------------------------

-spec validates_is_integer(Value :: any()) -> validates_result().

validates_is_integer(Value) when is_integer(Value) ->
    ?OK(Value);
validates_is_integer(Value) ->
    Msg = "Value must be an integer.",
    ?ERROR({is_integer, Value, Msg}).

%%------------------------------------------------------------------------------
%% @doc Validates is integer constructor.
%% @end
%%------------------------------------------------------------------------------

-spec is_integer_validation() -> validates().

is_integer_validation() -> fun validates_is_integer/1.

%%------------------------------------------------------------------------------
%% @doc Validates if the value is of float type.
%% @end
%%------------------------------------------------------------------------------

-spec validates_is_float(Value :: any()) -> validates_result().

validates_is_float(Value) when is_float(Value) ->
    ?OK(Value);
validates_is_float(Value) ->
    Msg = "Value must be a float.",
    ?ERROR({is_float, Value, Msg}).

%%------------------------------------------------------------------------------
%% @doc Validates if the value is of number type.
%% @end
%%------------------------------------------------------------------------------

-spec validates_is_number(Value :: any()) -> validates_result().

validates_is_number(Value) when is_number(Value) ->
    ?OK(Value);
validates_is_number(Value) ->
    Msg = "Value must be a number.",
    ?ERROR({is_number, Value, Msg}).

%%------------------------------------------------------------------------------
%% @doc Validates is number constructor.
%% @end
%%------------------------------------------------------------------------------

-spec is_number_validation() -> validates().

is_number_validation() -> fun validates_is_number/1.

%%------------------------------------------------------------------------------
%% @doc Validates if the value is of binary type.
%% @end
%%------------------------------------------------------------------------------

-spec validates_is_binary(Value :: any()) -> validates_result().

validates_is_binary(Value) when is_binary(Value) ->
    ?OK(Value);
validates_is_binary(Value) ->
    Msg = "Value must be a binary.",
    ?ERROR({is_binary, Value, Msg}).

%%------------------------------------------------------------------------------
%% @doc Validates is binary constructor.
%% @end
%%------------------------------------------------------------------------------

-spec is_binary_validation() -> validates().

is_binary_validation() -> fun validates_is_binary/1.

%%------------------------------------------------------------------------------
%% @doc Validates if the value is of boolean type.
%% @end
%%------------------------------------------------------------------------------

-spec validates_is_boolean(Value :: any()) -> validates_result().

validates_is_boolean(Value) when is_boolean(Value) ->
    ?OK(Value);
validates_is_boolean(Value) ->
    Msg = "Value must be a boolean.",
    ?ERROR({is_boolean, Value, Msg}).

%%------------------------------------------------------------------------------
%% @doc Validates is boolean constructor.
%% @end
%%------------------------------------------------------------------------------

-spec is_boolean_validation() -> validates().

is_boolean_validation() -> fun validates_is_boolean/1.

%%------------------------------------------------------------------------------
%% @doc Validates is float constructor.
%% @end
%%------------------------------------------------------------------------------

-spec is_float_validation() -> validates().

is_float_validation() -> fun validates_is_float/1.

%%------------------------------------------------------------------------------
%% @doc Validates if the value is of date type.
%% @end
%%------------------------------------------------------------------------------

-spec validates_is_date(Value :: any()) -> validates_result().

validates_is_date(Value) when ?is_date(Value) ->
    ?OK(Value);
validates_is_date(Value) ->
    Msg = "Value must be a date.",
    ?ERROR({is_date, Value, Msg}).

%%------------------------------------------------------------------------------
%% @doc Validates is date constructor.
%% @end
%%------------------------------------------------------------------------------

-spec is_date_validation() -> validates().

is_date_validation() -> fun validates_is_date/1.

%%------------------------------------------------------------------------------
%% @doc Validates if the value is of time type.
%% @end
%%------------------------------------------------------------------------------

-spec validates_is_time(Value :: any()) -> validates_result().

validates_is_time(Value) when ?is_time(Value) ->
    ?OK(Value);
validates_is_time(Value) ->
    Msg = "Value must be a time.",
    ?ERROR({is_time, Value, Msg}).

%%------------------------------------------------------------------------------
%% @doc Validates is time constructor.
%% @end
%%------------------------------------------------------------------------------

-spec is_time_validation() -> validates().

is_time_validation() -> fun validates_is_time/1.

%%------------------------------------------------------------------------------
%% @doc Validates if the value is of datetime type.
%% @end
%%------------------------------------------------------------------------------

-spec validates_is_datetime(Value :: any()) -> validates_result().

validates_is_datetime(Value) when ?is_datetime(Value) ->
    ?OK(Value);
validates_is_datetime(Value) ->
    Msg = "Value must be a datetime.",
    ?ERROR({is_datetime, Value, Msg}).

%%------------------------------------------------------------------------------
%% @doc Validates is datetime constructor.
%% @end
%%------------------------------------------------------------------------------

-spec is_datetime_validation() -> validates().

is_datetime_validation() -> fun validates_is_datetime/1.

%%------------------------------------------------------------------------------
%% @doc Validates number range.
%% @end
%%------------------------------------------------------------------------------

-spec validates_range(
    Value :: any(), Min :: integer(), Max :: integer()
) -> validates_result().

validates_range(Value, Min, Max) when ?in_range(Value, Min, Max) ->
    ?OK(Value);
validates_range(Value, Min, Max) ->
    Msg = lists:concat(["Value must a number between ", Min, " and ", Max, "."]),
    ?ERROR({range, Value, Msg}).

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
    Validations :: validations(), Value :: any()
) -> validates_result().

validate(Validations, Value) -> do_validate(Validations, Value, ?OK(Value)).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Process the validate/2 result.
%% @end
%%------------------------------------------------------------------------------

-spec do_validate(
    Validations :: validations(),
    Value :: any(),
    Result :: validates_result()
) -> validates_result().

do_validate([], _Value, Result) ->
    Result;
do_validate(_Validations, _Value, {error, _} = Error) ->
    Error;
do_validate([Validates | Validations], Value, _Result) ->
    Result = Validates(Value),
    do_validate(Validations, Value, Result).
