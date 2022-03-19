%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2022, williamthome
%%% @doc Schema for db module.
%%%
%%% @author William Fank Thomé
%%% @end
%%%-----------------------------------------------------------------------------
-module(todow_schema).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-type name() :: atom().
-type fields() :: nonempty_list(todow_field:t()).

-record(schema, {
    name :: name(),
    fields :: fields()
}).
-opaque t() :: #schema{}.

-export_type([
    t/0,
    name/0,
    fields/0
]).

-export([
    new/2,
    name/1,
    fields/1,
    cast/2, cast/3
]).

%%------------------------------------------------------------------------------
%% @doc Schema constructor.
%% @end
%%------------------------------------------------------------------------------

-spec new(Name :: name(), Fields :: fields()) -> t().

new(Name, Fields) -> #schema{name = Name, fields = Fields}.

%%------------------------------------------------------------------------------
%% @doc Get schema name.
%% @end
%%------------------------------------------------------------------------------

-spec name(Schema :: t()) -> name().

name(#schema{name = Name}) -> Name.

%%------------------------------------------------------------------------------
%% @doc Get schema fields.
%% @end
%%------------------------------------------------------------------------------

-spec fields(Schema :: t()) -> fields().

fields(#schema{fields = Fields}) -> Fields.

%%------------------------------------------------------------------------------
%% @doc Cast schema changes to changeset.
%% @end
%%------------------------------------------------------------------------------

-spec cast(Schema :: t(), Changes :: map()) -> {ok, changeset:t()}.

cast(Schema, Changes) -> cast(Schema, Changes, maps:new()).

%%------------------------------------------------------------------------------
%% @doc Cast schema changes to changeset.
%% @end
%%------------------------------------------------------------------------------

-spec cast(Schema :: t(), Data :: map(), Changes :: map()) -> {ok, changeset:t()}.

cast(Schema, Data, Changes) ->
    todow_changeset:cast(
        Data,
        Changes,
        not_private_fields_name(Schema),
        #{defaults => defaults(Schema)}
    ).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Get schema defaults.
%% @end
%%------------------------------------------------------------------------------

-spec defaults(Schema :: t()) -> map().

defaults(Schema) ->
    lists:foldl(
        fun(Field, Defaults) ->
            case todow_field:default(Field) of
                undefined -> Defaults;
                Default -> maps:put(todow_field:name(Field), Default, Defaults)
            end
        end,
        #{},
        fields(Schema)
    ).

%%------------------------------------------------------------------------------
%% @doc Filter schema fields.
%% @end
%%------------------------------------------------------------------------------

-spec filtermap_fields(
    BooleanCallback :: fun((Field :: todow_field:t()) -> boolean()),
    Schema :: t()
) -> fields().

filtermap_fields(BooleanCallback, Schema) ->
    lists:filtermap(
        fun(Field) ->
            case BooleanCallback(Field) of
                true -> {true, Field};
                false -> false
            end
        end,
        fields(Schema)
    ).

%%------------------------------------------------------------------------------
%% @doc Get schema not private fields.
%% @end
%%------------------------------------------------------------------------------

-spec not_private_fields(Schema :: t()) -> fields().

not_private_fields(Schema) ->
    filtermap_fields(
        fun(Field) -> not todow_field:private(Field) end,
        Schema
    ).

%%------------------------------------------------------------------------------
%% @doc Get schema not private fields name.
%% @end
%%------------------------------------------------------------------------------

-spec not_private_fields_name(Schema :: t()) -> list(todow_field:name()).

not_private_fields_name(Schema) ->
    lists:map(
        fun(Field) -> todow_field:name(Field) end,
        not_private_fields(Schema)
    ).

%%%=============================================================================
%%% Tests
%%%=============================================================================

-ifdef(TEST).

new_test() ->
    ?assertEqual(new(foo, []), #schema{name = foo, fields = []}).

not_private_fields_test() ->
    NotPrivateField = todow_field:new(bar, binary, #{private => false}),
    Schema = #schema{
        name = foo,
        fields = [
            todow_field:new(foo, binary, #{private => true}),
            NotPrivateField,
            todow_field:new(baz, binary, #{private => true})
        ]
    },
    ?assertEqual([NotPrivateField], not_private_fields(Schema)).

not_private_fields_name_test() ->
    Schema = #schema{
        name = foo,
        fields = [
            todow_field:new(foo, binary, #{private => true}),
            todow_field:new(bar, binary, #{private => false}),
            todow_field:new(baz, binary, #{private => true})
        ]
    },
    ?assertEqual([bar], not_private_fields_name(Schema)).

defaults_test() ->
    Schema = #schema{
        name = foo,
        fields = [
            todow_field:new(foo, binary, #{default => foo}),
            todow_field:new(bar, binary),
            todow_field:new(baz, binary, #{default => baz})
        ]
    },
    ?assertEqual(#{foo => foo, baz => baz}, defaults(Schema)).

-endif.
