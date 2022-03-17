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

-type type() :: integer | binary | date | boolean.

-record(field, {
    name :: atom(),
    type :: type(),
    private = false :: boolean(),
    required = false :: boolean(),
    default = undefined :: any(),
    validations = [] :: list(todow_validation:validation())
}).
-opaque t() :: #field{}.

-export_type([t/0, type/0]).
-export([new/2, new/3]).

-define(DEFAULT_OPTIONS, #{
    private => false,
    required => false,
    default => undefined,
    validations => []
}).

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
