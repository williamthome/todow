%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2022, williamthome
%%% @doc Field for schema module.
%%%
%%% @author William Fank Thomé
%%% @end
%%%-----------------------------------------------------------------------------
-module(todow_field).

-include("./include/todow.hrl").
-include("./include/todow_field.hrl").

-define(DEFAULT_PRIVATE, false).
-define(DEFAULT_REQUIRED, false).
-define(DEFAULT_VALUE, undefined).
-define(DEFAULT_VALIDATORS, []).

-define(DEFAULT_OPTIONS, #{
    private => ?DEFAULT_PRIVATE,
    required => ?DEFAULT_REQUIRED,
    default => ?DEFAULT_VALUE,
    validators => ?DEFAULT_VALIDATORS
}).

-type name() :: atom().
-type type() ::
    atom()
    | ?integer
    | ?float
    | ?number
    | ?binary
    | ?boolean
    | ?date
    | ?time
    | ?datetime
    | ?id.
-type private() :: boolean().
-type required() :: boolean().
-type default() :: any().
-type validators() :: todow_validation:validators().

-record(field, {
    name :: name(),
    type :: type(),
    private = ?DEFAULT_PRIVATE :: private(),
    required = ?DEFAULT_REQUIRED :: required(),
    default = ?DEFAULT_VALUE :: default(),
    validators = ?DEFAULT_VALIDATORS :: validators()
}).
-opaque t() :: #field{}.

-export_type([
    t/0,
    name/0,
    type/0,
    private/0,
    required/0,
    default/0,
    validators/0
]).

-export([
    new/2, new/3,
    new_id/0,
    new_timestamp_created_at/0,
    new_timestamp_updated_at/0,
    name/1,
    type/1,
    private/1,
    required/1,
    default/1,
    validators/1
]).

-export([
    validates/2,
    validates_changeset/3
]).

%%------------------------------------------------------------------------------
%% @doc Field constructor.
%% @end
%%------------------------------------------------------------------------------
-spec new(Name :: name(), Type :: type()) -> t().

new(Name, Type) -> new(Name, Type, maps:new()).

%%------------------------------------------------------------------------------
%% @doc Field constructor.
%% @end
%%------------------------------------------------------------------------------
-spec new(Name :: name(), Type :: type(), Options :: map()) -> t().

new(Name, Type, Options) ->
    Args = #{
        name => Name,
        type => Type
    },
    do_new(maps:merge(do_options(Options), Args)).

%%------------------------------------------------------------------------------
%% @doc Field id constructor.
%% @end
%%------------------------------------------------------------------------------
-spec new_id() -> t().

new_id() -> new(id, ?integer, #{private => true}).

%%------------------------------------------------------------------------------
%% @doc Field created_at constructor.
%% @end
%%------------------------------------------------------------------------------
-spec new_timestamp_created_at() -> t().

new_timestamp_created_at() ->
    new(
        created_at,
        ?date,
        #{
            default => fun(Changeset) ->
                case todow_changeset:is_action_new(Changeset) of
                    true -> calendar:universal_time();
                    false -> todow_changeset:ignore_default()
                end
            end
        }
    ).

%%------------------------------------------------------------------------------
%% @doc Field updated_at constructor.
%% @end
%%------------------------------------------------------------------------------
-spec new_timestamp_updated_at() -> t().

new_timestamp_updated_at() ->
    new(
        updated_at,
        ?date,
        #{default => fun calendar:universal_time/0}
    ).

%%------------------------------------------------------------------------------
%% @doc Get field name.
%% @end
%%------------------------------------------------------------------------------
-spec name(Field :: t()) -> name().

name(#field{name = Name}) -> Name.

%%------------------------------------------------------------------------------
%% @doc Get field type.
%% @end
%%------------------------------------------------------------------------------
-spec type(Field :: t()) -> type().

type(#field{type = Type}) -> Type.

%%------------------------------------------------------------------------------
%% @doc Get field private.
%% @end
%%------------------------------------------------------------------------------
-spec private(Field :: t()) -> private().

private(#field{private = Private}) -> Private.

%%------------------------------------------------------------------------------
%% @doc Get field required.
%% @end
%%------------------------------------------------------------------------------
-spec required(Field :: t()) -> required().

required(#field{required = Required}) -> Required.

%%------------------------------------------------------------------------------
%% @doc Get field default.
%% @end
%%------------------------------------------------------------------------------
-spec default(Field :: t()) -> default().

default(#field{default = Default}) -> Default.

%%------------------------------------------------------------------------------
%% @doc Get field validators.
%% @end
%%------------------------------------------------------------------------------
-spec validators(Field :: t()) -> validators().

validators(#field{validators = Validators}) -> Validators.

%%------------------------------------------------------------------------------
%% @doc Validates field value.
%% @end
%%------------------------------------------------------------------------------
-spec validates(
    Field :: t(), Value :: any()
) -> todow_validation:validates_result().

validates(#field{validators = Validators}, Value) ->
    todow_validation:validates(Validators, Value).

%%------------------------------------------------------------------------------
%% @doc Validates changeset by field and value.
%% @end
%%------------------------------------------------------------------------------
-spec validates_changeset(
    Changeset :: changeset:t(), Field :: t(), Value :: any()
) -> changeset:t().

validates_changeset(Changeset, Field = #field{name = Key}, Value) ->
    Validators = do_validators(Field, Value),
    todow_changeset:validates(Changeset, Validators, Key, Value).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Field constructor.
%% @end
%%------------------------------------------------------------------------------
-spec do_new(Args :: map()) -> t().

do_new(#{
    name := Name,
    type := Type,
    private := Private,
    required := Required,
    default := Default,
    validators := Validators
}) ->
    #field{
        name = Name,
        type = Type,
        private = Private,
        required = Required,
        default = Default,
        validators = Validators
    }.

%%------------------------------------------------------------------------------
%% @doc Do validators.
%% @end
%%------------------------------------------------------------------------------
-spec do_validators(Field :: t(), Value :: any()) -> validators().

do_validators(
    #field{required = false, validators = Validators},
    _Value = undefined
) ->
    Validators;
do_validators(
    Field = #field{validators = Validators},
    _Value
) ->
    Validators0 = add_type_validator(Field, Validators),
    maybe_add_required_validator(Field, Validators0).

%%------------------------------------------------------------------------------
%% @doc Maybe add the type validator to validators list.
%% @end
%%------------------------------------------------------------------------------
-spec add_type_validator(
    FieldOrType :: t() | type(), Validators :: validators()
) -> validators().

add_type_validator(FieldOrType, Validators) ->
    [get_type_validator(FieldOrType) | Validators].

%%------------------------------------------------------------------------------
%% @doc If valid type, returns the type validator.
%% @end
%%------------------------------------------------------------------------------
-spec get_type_validator(
    FieldOrType :: t() | type()
) -> todow_validation:validates() | undefined.

get_type_validator(#field{type = Type}) -> get_type_validator(Type);
get_type_validator(?integer) -> todow_validation:is_integer_validator();
get_type_validator(?float) -> todow_validation:is_float_validator();
get_type_validator(?number) -> todow_validation:is_number_validator();
get_type_validator(?binary) -> todow_validation:is_binary_validator();
get_type_validator(?boolean) -> todow_validation:is_boolean_validator();
get_type_validator(?date) -> todow_validation:is_date_validator();
get_type_validator(?time) -> todow_validation:is_time_validator();
get_type_validator(?datetime) -> todow_validation:is_datetime_validator();
get_type_validator(_Type) -> undefined.

%%------------------------------------------------------------------------------
%% @doc Maybe add required validation to validators list.
%% @end
%%------------------------------------------------------------------------------
-spec maybe_add_required_validator(
    FieldOrBoolean :: t() | boolean(), Validators :: validators()
) -> validators().

maybe_add_required_validator(#field{required = Required}, Validators) ->
    maybe_add_required_validator(Required, Validators);
maybe_add_required_validator(_Required = true, Validators) ->
    [todow_validation:required_validator() | Validators];
maybe_add_required_validator(_Required = false, Validators) ->
    Validators.

%%------------------------------------------------------------------------------
%% @doc Merge options with default options.
%% @end
%%------------------------------------------------------------------------------
-spec do_options(Options :: map()) -> map().

do_options(Options) -> maps:merge(?DEFAULT_OPTIONS, Options).

%%%=============================================================================
%%% Tests
%%%=============================================================================

-ifdef(TEST).

new_test() ->
    ?assertEqual(
        new(foo, ?binary),
        #field{
            name = foo,
            type = ?binary
        }
    ),
    ?assertEqual(
        new(foo, ?binary, #{default => <<"bar">>}),
        #field{
            name = foo,
            type = ?binary,
            default = <<"bar">>
        }
    ).

-endif.
