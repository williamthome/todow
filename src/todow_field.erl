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
-define(DEFAULT_VALIDATIONS, []).

-define(DEFAULT_OPTIONS, #{
    private => ?DEFAULT_PRIVATE,
    required => ?DEFAULT_REQUIRED,
    default => ?DEFAULT_VALUE,
    validations => ?DEFAULT_VALIDATIONS
}).

-type name() :: atom().
-type type() :: ?integer | ?binary | ?date | ?boolean.
-type private() :: boolean().
-type required() :: boolean().
-type default() :: any().
-type validations() :: todow_validation:validations().

-record(field, {
    name :: name(),
    type :: type(),
    private = ?DEFAULT_PRIVATE :: private(),
    required = ?DEFAULT_REQUIRED :: required(),
    default = ?DEFAULT_VALUE :: default(),
    validations = ?DEFAULT_VALIDATIONS :: validations()
}).
-opaque t() :: #field{}.

-export_type([
    t/0,
    name/0,
    type/0,
    private/0,
    required/0,
    default/0,
    validations/0
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
    validations/1
]).

-export([
    validate/2,
    validate_changeset/3
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
%% @doc Get field validations.
%% @end
%%------------------------------------------------------------------------------

-spec validations(Field :: t()) -> validations().

validations(#field{validations = Validations}) -> Validations.

%%------------------------------------------------------------------------------
%% @doc Validates field value.
%% @end
%%------------------------------------------------------------------------------

-spec validate(
    Field :: t(), Value :: any()
) -> todow_validation:validates_result().

validate(#field{validations = Validations}, Value) ->
    todow_validation:validate(Validations, Value).

%%------------------------------------------------------------------------------
%% @doc Validates changeset by field and value.
%% @end
%%------------------------------------------------------------------------------

-spec validate_changeset(
    Changeset :: changeset:t(), Field :: t(), Value :: any()
) -> changeset:t().

validate_changeset(Changeset, Field = #field{name = Key}, Value) ->
    Validations = do_validations(Field, Value),
    todow_changeset:validate(Changeset, Validations, Key, Value).

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
    validations := Validations
}) ->
    #field{
        name = Name,
        type = Type,
        private = Private,
        required = Required,
        default = Default,
        validations = Validations
    }.

%%------------------------------------------------------------------------------
%% @doc Do validations.
%% @end
%%------------------------------------------------------------------------------

-spec do_validations(Field :: t(), Value :: any()) -> validations().

do_validations(
    #field{required = false, validations = Validations},
    _Value = undefined
) ->
    Validations;
do_validations(
    Field = #field{validations = Validations},
    _Value
) ->
    Validations0 = maybe_add_type_validation(Field, Validations),
    maybe_add_required_validation(Field, Validations0).

maybe_add_type_validation(FieldOrType, Validations) ->
    case get_type_validation(FieldOrType) of
        undefined -> Validations;
        TypeValidation -> [TypeValidation | Validations]
    end.

get_type_validation(#field{type = Type}) -> get_type_validation(Type);
get_type_validation(?integer) -> todow_validation:is_integer_validation();
get_type_validation(?float) -> todow_validation:is_float_validation();
get_type_validation(?number) -> todow_validation:is_number_validation();
get_type_validation(?binary) -> todow_validation:is_binary_validation();
get_type_validation(?boolean) -> todow_validation:is_boolean_validation();
get_type_validation(?date) -> todow_validation:is_date_validation();
get_type_validation(?time) -> todow_validation:is_time_validation();
get_type_validation(?datetime) -> todow_validation:is_datetime_validation();
get_type_validation(_Unknown) -> undefined.

maybe_add_required_validation(#field{required = Required}, Validations) ->
    maybe_add_required_validation(Required, Validations);
maybe_add_required_validation(_Required = true, Validations) ->
    [todow_validation:required_validation() | Validations];
maybe_add_required_validation(_Required = false, Validations) ->
    Validations.

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
