-module(todow_param).

-include("./include/todow.hrl").

-define(PARAM_DEFAULTS, #{
    default => undefined,
    validators => []
}).

-define(is_param(X), is_map(X)).

-type name() :: atom().
% TODO: More types, e.g. string, param, topic, etc, according to app objects
-type type() ::
    atom
    | integer
    | float
    | number
    | binary
    | boolean
    | date
    | time
    | datetime
    | currency.
% TODO: More error codes
-type validation_error_code() ::
    bad_arg
    | unauthorized
    | forbidden
    | internal_server_error.
-type validation_error_reason() :: atom().
-type validation_error() :: {
    Code :: validation_error_code(),
    Reason :: validation_error_reason(),
    Value :: any()
}.
-type validates() :: fun((1) -> ok | {error, validation_error()}).
-type validators() :: validates() | list(validates()).
-type default() :: fun(() -> any()) | any().

% TODO: Cast?
% TODO: Secret flag to hide value on validation_error
-type t() :: #{
    name => name(),
    type => type(),
    default => default(),
    validators => validators()
}.

-export_type([
    t/0
]).

-export([
    new/1,
    merge/2
]).

-spec new(Args :: map()) -> t().

% TODO: Add validators by type
new(Args = #{name := _Name, type := Type}) ->
    Defaults = ?PARAM_DEFAULTS#{type => Type},
    todow_utils:factory(Args, Defaults, fun merge/2).

-spec merge(A :: map(), B :: map()) -> t().

merge(A = #{type := Type}, B = #{type := Type}) ->
    A#{
        default => merge_default(A, B),
        validators => merge_validators(A, B)
    }.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

-spec unwrap_default(Def :: any()) -> any().

unwrap_default(#{default := Def}) -> unwrap_default(Def);
unwrap_default(Def) when is_function(Def, 0) -> Def();
unwrap_default(Def) -> Def.

-spec merge_default(DefA :: map(), DefB :: map()) -> any().

merge_default(#{default := DefA}, _DefB) when DefA =/= undefined ->
    DefA;
merge_default(#{}, #{default := DefB}) ->
    DefB;
merge_default(A, B) ->
    fun() ->
        case unwrap_default(A) of
            undefined -> unwrap_default(B);
            Def -> Def
        end
    end.

-spec merge_validators(A :: t(), B :: t()) -> validators().

merge_validators(A, B) ->
    GetValidators = todow_utils:wrap_get(validators, []),
    todow_utils:merge_to_list(GetValidators(B), GetValidators(A)).

%%%=============================================================================
%%% Tests
%%%=============================================================================

-ifdef(TEST).

unwrap_default_test() ->
    ?assertEqual(foo, unwrap_default(#{default => foo})),
    ?assertEqual(foo, unwrap_default(#{default => fun() -> foo end})).

merge_default_test() ->
    Def1 = merge_default(#{default => foo}, #{default => fun() -> bar end}),
    Def2 = merge_default(#{default => undefined}, #{default => fun() -> bar end}),
    ?assertEqual(foo, unwrap_default(Def1)),
    ?assertEqual(bar, unwrap_default(Def2)).

merge_validators_test() ->
    V1 = fun(_) -> ok end,
    V2 = fun(X) -> {error, {unknown, v2_error, X}} end,
    V3 = fun(X) -> {error, {unknown, v3_error, X}} end,
    A = #{validators => V1},
    B = #{validators => [V2, V3]},
    ?assertEqual([V2, V3, V1], merge_validators(A, B)).

merge_test() ->
    V1 = fun(_) -> ok end,
    V2 = fun(X) -> {error, {unknown, v2_error, X}} end,
    V3 = fun(X) -> {error, {unknown, v3_error, X}} end,
    A = #{
        name => foo,
        type => any_type,
        default => undefined,
        validators => V1
    },
    B = #{
        name => bar,
        type => any_type,
        default => baz,
        validators => [V2, V3]
    },
    Expected = #{
        name => foo,
        type => any_type,
        default => baz,
        validators => [V2, V3, V1]
    },
    ?assertEqual(Expected, merge(A, B)).

new_test() ->
    Args = #{
        name => foo,
        type => any_type
    },
    Expected = #{
        name => foo,
        type => any_type,
        default => undefined,
        validators => []
    },
    ?assertEqual(Expected, new(Args)).

-endif.
