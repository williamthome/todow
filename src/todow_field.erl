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

-import(todow_validation, [
    validates_required/1
]).

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

-type type() :: integer | binary | date | boolean.

-record(field, {
    name :: atom(),
    type :: type(),
    private = ?DEFAULT_PRIVATE :: boolean(),
    required = ?DEFAULT_REQUIRED :: boolean(),
    default = ?DEFAULT_VALUE :: any(),
    validations = ?DEFAULT_VALIDATIONS :: list(todow_validation:validation())
}).
-opaque t() :: #field{}.

-export_type([t/0, type/0]).
-export([new/2, new/3]).

%%------------------------------------------------------------------------------
%% @doc Field constructor.
%% @end
%%------------------------------------------------------------------------------

-spec new(Name :: atom(), Type :: type()) -> t().

new(Name, Type) ->
    new(Name, Type, maps:new()).

%%------------------------------------------------------------------------------
%% @doc Field constructor.
%% @end
%%------------------------------------------------------------------------------

-spec new(Name :: atom(), Type :: type(), Options :: map()) -> t().

new(Name, Type, Options) ->
    Required = #{name => Name, type => Type},
    Args = maps:merge(Required, do_options(Options)),
    do_new(Args).

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
