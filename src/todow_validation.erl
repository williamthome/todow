-module(todow_validation).

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

-type validation_error_reason() :: atom().
-type validation_error(Value) ::
    {Reason :: validation_error_reason(), Value}.
-type validation_error_result(Value) ::
    todow:error_result(validation_error(Value)).
-type validation_result(Value) ::
    todow:result(Value, validation_error(Value)).
-type validates() :: fun((Value) -> validation_result(Value)).
-type validators() :: list(validates()).

-export_type([
    validation_error/1,
    validation_error_result/1,
    validation_result/1,
    validates/0,
    validators/0
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
    required_validator/0,
    is_integer_validator/0,
    is_float_validator/0,
    is_number_validator/0,
    is_binary_validator/0,
    is_boolean_validator/0,
    is_date_validator/0,
    is_time_validator/0,
    is_datetime_validator/0,
    range_validator/2
]).
-export([
    validates/2,
    make_error/3, make_error/4
]).

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
-spec validates_required(Value) -> validation_result(Value).

validates_required(Value) when ?is_defined(Value) ->
    todow:ok(Value);
validates_required(Value) ->
    Msg = "Value is required",
    make_error(required, Value, Msg).

%%------------------------------------------------------------------------------
%% @doc Validates required constructor.
%% @end
%%------------------------------------------------------------------------------
-spec required_validator() -> validates().

required_validator() -> fun validates_required/1.

%%------------------------------------------------------------------------------
%% @doc Validates if the value is of integer type.
%% @end
%%------------------------------------------------------------------------------
-spec validates_is_integer(Value) -> validation_result(Value).

validates_is_integer(Value) when is_integer(Value) ->
    todow:ok(Value);
validates_is_integer(Value) ->
    Msg = "Value must be an integer.",
    make_error(is_integer, Value, Msg).

%%------------------------------------------------------------------------------
%% @doc Validates is integer constructor.
%% @end
%%------------------------------------------------------------------------------
-spec is_integer_validator() -> validates().

is_integer_validator() -> fun validates_is_integer/1.

%%------------------------------------------------------------------------------
%% @doc Validates if the value is of float type.
%% @end
%%------------------------------------------------------------------------------
-spec validates_is_float(Value) -> validation_result(Value).

validates_is_float(Value) when is_float(Value) ->
    todow:ok(Value);
validates_is_float(Value) ->
    Msg = "Value must be a float.",
    make_error(is_float, Value, Msg).

%%------------------------------------------------------------------------------
%% @doc Validates if the value is of number type.
%% @end
%%------------------------------------------------------------------------------
-spec validates_is_number(Value) -> validation_result(Value).

validates_is_number(Value) when is_number(Value) ->
    todow:ok(Value);
validates_is_number(Value) ->
    Msg = "Value must be a number.",
    make_error(is_number, Value, Msg).

%%------------------------------------------------------------------------------
%% @doc Validates is number constructor.
%% @end
%%------------------------------------------------------------------------------
-spec is_number_validator() -> validates().

is_number_validator() -> fun validates_is_number/1.

%%------------------------------------------------------------------------------
%% @doc Validates if the value is of binary type.
%% @end
%%------------------------------------------------------------------------------
-spec validates_is_binary(Value) -> validation_result(Value).

validates_is_binary(Value) when is_binary(Value) ->
    todow:ok(Value);
validates_is_binary(Value) ->
    Msg = "Value must be a binary.",
    make_error(is_binary, Value, Msg).

%%------------------------------------------------------------------------------
%% @doc Validates is binary constructor.
%% @end
%%------------------------------------------------------------------------------
-spec is_binary_validator() -> validates().

is_binary_validator() -> fun validates_is_binary/1.

%%------------------------------------------------------------------------------
%% @doc Validates if the value is of boolean type.
%% @end
%%------------------------------------------------------------------------------
-spec validates_is_boolean(Value) -> validation_result(Value).

validates_is_boolean(Value) when is_boolean(Value) ->
    todow:ok(Value);
validates_is_boolean(Value) ->
    Msg = "Value must be a boolean.",
    make_error(is_boolean, Value, Msg).

%%------------------------------------------------------------------------------
%% @doc Validates is boolean constructor.
%% @end
%%------------------------------------------------------------------------------
-spec is_boolean_validator() -> validates().

is_boolean_validator() -> fun validates_is_boolean/1.

%%------------------------------------------------------------------------------
%% @doc Validates is float constructor.
%% @end
%%------------------------------------------------------------------------------
-spec is_float_validator() -> validates().

is_float_validator() -> fun validates_is_float/1.

%%------------------------------------------------------------------------------
%% @doc Validates if the value is of date type.
%% @end
%%------------------------------------------------------------------------------
-spec validates_is_date(Value) -> validation_result(Value).

validates_is_date(Value) when ?is_date(Value) ->
    todow:ok(Value);
validates_is_date(Value) ->
    Msg = "Value must be a date.",
    make_error(is_date, Value, Msg).

%%------------------------------------------------------------------------------
%% @doc Validates is date constructor.
%% @end
%%------------------------------------------------------------------------------
-spec is_date_validator() -> validates().

is_date_validator() -> fun validates_is_date/1.

%%------------------------------------------------------------------------------
%% @doc Validates if the value is of time type.
%% @end
%%------------------------------------------------------------------------------
-spec validates_is_time(Value) -> validation_result(Value).

validates_is_time(Value) when ?is_time(Value) ->
    todow:ok(Value);
validates_is_time(Value) ->
    Msg = "Value must be a time.",
    make_error(is_time, Value, Msg).

%%------------------------------------------------------------------------------
%% @doc Validates is time constructor.
%% @end
%%------------------------------------------------------------------------------
-spec is_time_validator() -> validates().

is_time_validator() -> fun validates_is_time/1.

%%------------------------------------------------------------------------------
%% @doc Validates if the value is of datetime type.
%% @end
%%------------------------------------------------------------------------------
-spec validates_is_datetime(Value) -> validation_result(Value).

validates_is_datetime(Value) when ?is_datetime(Value) ->
    todow:ok(Value);
validates_is_datetime(Value) ->
    Msg = "Value must be a datetime.",
    make_error(is_datetime, Value, Msg).

%%------------------------------------------------------------------------------
%% @doc Validates is datetime constructor.
%% @end
%%------------------------------------------------------------------------------
-spec is_datetime_validator() -> validates().

is_datetime_validator() -> fun validates_is_datetime/1.

%%------------------------------------------------------------------------------
%% @doc Validates number range.
%% @end
%%------------------------------------------------------------------------------
-spec validates_range(
    Value,
    Min :: integer(),
    Max :: integer()
) -> validation_result(Value).

validates_range(Value, Min, Max) when ?in_range(Value, Min, Max) ->
    todow:ok(Value);
validates_range(Value, Min, Max) ->
    Msg = lists:concat(["Value must a number between ", Min, " and ", Max, "."]),
    make_error(range, Value, Msg).

%%------------------------------------------------------------------------------
%% @doc Validates range constructor.
%% @end
%%------------------------------------------------------------------------------
-spec range_validator(Min :: integer(), Max :: integer()) -> validates().

range_validator(Min, Max) ->
    fun(Value) -> validates_range(Value, Min, Max) end.

%%------------------------------------------------------------------------------
%% @doc Validate by recursion. Halts on the first error.
%% @end
%%------------------------------------------------------------------------------
-spec validates(
    Validators :: validators(),
    Value
) -> validation_result(Value).

validates(Validators, Value) -> do_validates(Validators, Value, todow:ok(Value)).

%%------------------------------------------------------------------------------
%% @doc Gen validation error.
%% @end
%%------------------------------------------------------------------------------
-spec make_error(
    Reason :: validation_error_reason(),
    Value,
    Msg :: todow:error_msg()
) -> validation_error_result(Value).

make_error(Reason, Value, Msg) -> make_error(bad_arg, Reason, Value, Msg).

%%------------------------------------------------------------------------------
%% @doc Gen validation error.
%% @end
%%------------------------------------------------------------------------------
-spec make_error(
    Code :: todow:error_code(),
    Reason :: validation_error_reason(),
    Value,
    Msg :: todow:error_msg()
) -> validation_error_result(Value).

make_error(Code, Reason, Value, Msg) ->
    todow:error(Code, {Reason, Value}, Msg).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Process the validates/2 result.
%% @end
%%------------------------------------------------------------------------------
-spec do_validates(
    Validators :: validators(),
    Value,
    Result :: validation_result(Value)
) -> validation_result(Value).

do_validates([], _Value, Result) ->
    Result;
do_validates(_Validators, _Value, {error, _} = Error) ->
    Error;
do_validates([Validates | Validators], Value, _Result) ->
    Result = Validates(Value),
    do_validates(Validators, Value, Result).
