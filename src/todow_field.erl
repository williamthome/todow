%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2022, williamthome
%%% @doc Field for schema module.
%%%
%%% @author William Fank Thomé
%%% @end
%%%-----------------------------------------------------------------------------
-module(todow_field).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

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
-type type() :: integer | binary | date | boolean.
-type private() :: boolean().
-type required() :: boolean().
-type default() :: any().
-type validations() :: list(todow_validation:validates()).

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
    name/1,
    type/1,
    private/1,
    required/1,
    default/1,
    validations/1
]).

-export([
    validate/2
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
        type => Type,
        validations => do_validations(Options)
    },
    do_new(maps:merge(Args, do_options(Options))).

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

validate(#field{validations = Validations}, Value) ->
    todow_validation:validate(Validations, Value).

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
%% @doc Default validations.
%% @end
%%------------------------------------------------------------------------------

-spec do_validations(Options :: map()) -> validations().

do_validations(#{required := true}) ->
    [todow_validation:required_validation() | ?DEFAULT_VALIDATIONS];
do_validations(_Options) -> ?DEFAULT_VALIDATIONS.

%%------------------------------------------------------------------------------
%% @doc Merge options with default options.
%% @end
%%------------------------------------------------------------------------------

-spec do_options(Options :: map()) -> map().

do_options(Options) ->
    maps:merge(?DEFAULT_OPTIONS, Options).

%%%=============================================================================
%%% Tests
%%%=============================================================================

-ifdef(TEST).

new_test() ->
    ?assertEqual(new(foo, binary), #field{name = foo, type = binary}),
    ?assertEqual(
        new(foo, binary, #{default => bar}),
        #field{name = foo, type = binary, default = bar}
    ).

-endif.
