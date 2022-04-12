%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2022, williamthome
%%% @doc Schema for db module.
%%%
%%% @author William Fank Thomé
%%% @end
%%%-----------------------------------------------------------------------------
-module(todow_schema_core).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-type name() :: atom().
-type fields() :: nonempty_list(todow_field:t()).
-type field() ::
    {ok, todow_field:t()} | {error, {not_found, todow_field:name()}}.
-type validates() ::
    {ok, todow_changeset:t()} | {error, todow_changeset:errors()}.

-record(schema, {
    name :: name(),
    fields :: fields()
}).
-opaque t() :: #schema{}.

-export_type([
    t/0,
    name/0,
    fields/0,
    field/0,
    validates/0
]).

-export([
    new/2, new/3,
    name/1,
    fields/1,
    field_names/1
]).

-export([
    field/2,
    is_field/2,
    cast/2, cast/3,
    validates/2, validates/3
]).

%%------------------------------------------------------------------------------
%% @doc Schema constructor.
%% @end
%%------------------------------------------------------------------------------
-spec new(Name :: name(), Fields :: fields()) -> t().

new(Name, Fields) ->
    new(Name, Fields, maps:new()).

%%------------------------------------------------------------------------------
%% @doc Schema constructor.
%% @end
%%------------------------------------------------------------------------------
-spec new(Name :: name(), Fields :: fields(), Options :: map()) -> t().

new(Name, Fields, Options) ->
    #schema{
        name = Name,
        fields = do_fields(Fields, Options)
    }.

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
%% @doc Get schema field names.
%% @end
%%------------------------------------------------------------------------------
-spec field_names(Schema :: t()) -> list(todow_field:name()).

field_names(Schema) ->
    lists:map(
        fun todow_field:name/1,
        fields(Schema)
    ).

%%------------------------------------------------------------------------------
%% @doc Get schema field by field name.
%% @end
%%------------------------------------------------------------------------------
-spec field(Schema :: t(), FieldName :: todow_field:name()) -> field().

field(#schema{fields = Fields}, FieldName) ->
    case
        lists:dropwhile(
            fun(Field) -> todow_field:name(Field) =/= FieldName end,
            Fields
        )
    of
        [] -> {error, {not_found, FieldName}};
        [Field | _] -> {ok, Field}
    end.

%%------------------------------------------------------------------------------
%% @doc Returns true if field name it's a schema field.
%% @end
%%------------------------------------------------------------------------------
-spec is_field(Schema :: t(), FieldName :: todow_field:name()) -> boolean().

is_field(Schema, FieldName) ->
    field(Schema, FieldName) =/= {error, {not_found, FieldName}}.

%%------------------------------------------------------------------------------
%% @doc Cast schema changes to changeset.
%% @end
%%------------------------------------------------------------------------------
-spec cast(Schema :: t(), Changes :: map()) -> {ok, todow_changeset:t()}.

cast(Schema, Changes) -> cast(Schema, Changes, maps:new()).

%%------------------------------------------------------------------------------
%% @doc Cast schema changes to changeset.
%% @end
%%------------------------------------------------------------------------------
-spec cast(
    Schema :: t(), Data :: map(), Changes :: map()
) -> {ok, todow_changeset:t()}.

cast(Schema, Data, Changes) ->
    todow_changeset:cast(
        Data,
        Changes,
        not_private_fields_name(Schema),
        #{defaults => defaults(Schema)}
    ).

%%------------------------------------------------------------------------------
%% @doc Validates schema.
%% @end
%%------------------------------------------------------------------------------
-spec validates(Schema :: t(), Data :: map(), Changes :: map()) -> validates().

validates(Schema, Data, Changes) ->
    {ok, Changeset} = cast(Schema, Data, Changes),
    validates(Schema, Changeset).

%%------------------------------------------------------------------------------
%% @doc Validates schema.
%% @end
%%------------------------------------------------------------------------------
-spec validates(Schema :: t(), Changeset :: todow_changeset:t()) -> validates().

validates(Schema, Changeset) ->
    ValidChanges = todow_changeset:valid_changes(Changeset),
    ChangesetValidated = maps:fold(
        fun(Key, Value, ChangesetAcc) ->
            case field(Schema, Key) of
                {ok, Field} ->
                    todow_field:validates_changeset(ChangesetAcc, Field, Value);
                {error, {not_found, _FieldName}} ->
                    ChangesetAcc
            end
        end,
        Changeset,
        ValidChanges
    ),
    case not todow_changeset:with_errors(ChangesetValidated) of
        true -> {ok, ChangesetValidated};
        false -> {error, ChangesetValidated}
    end.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Create fields by options.
%% @end
%%------------------------------------------------------------------------------
-spec do_fields(Fields :: fields(), Options :: map()) -> fields().

do_fields(Fields, Options) ->
    maybe_add_timestamp_fields(Fields, Options).
-spec maybe_add_timestamp_fields(
    Fields :: fields(),
    Options :: map()
) -> fields().

maybe_add_timestamp_fields(Fields, #{timestamps := true}) ->
    CreatedAt = todow_field:new_timestamp_created_at(),
    UpdatedAt = todow_field:new_timestamp_updated_at(),
    [CreatedAt, UpdatedAt | Fields];
maybe_add_timestamp_fields(Fields, _Options) ->
    Fields.

%%------------------------------------------------------------------------------
%% @doc Get schema defaults.
%% @end
%%------------------------------------------------------------------------------
-spec defaults(Schema :: t()) -> map().

defaults(Schema) ->
    lists:foldl(
        fun(Field, Defaults) ->
            maps:put(
                todow_field:name(Field),
                todow_field:default(Field),
                Defaults
            )
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
            todow_field:new(foo, binary, #{default => <<"foo">>}),
            todow_field:new(bar, binary),
            todow_field:new(baz, binary, #{default => <<"baz">>})
        ]
    },
    ?assertEqual(
        #{foo => <<"foo">>, bar => undefined, baz => <<"baz">>},
        defaults(Schema)
    ).

validates_test() ->
    Schema = #schema{
        name = foo,
        fields = [
            todow_field:new(foo, binary, #{default => <<"foo">>}),
            todow_field:new(bar, binary),
            todow_field:new(baz, binary, #{default => <<"baz">>})
        ]
    },
    ?assertEqual(
        todow_changeset:new(
            #{foo => <<"bar">>, bar => undefined, baz => <<"baz">>},
            #{foo => <<"bar">>, bar => undefined, baz => <<"baz">>},
            new
        ),
        validates(Schema, maps:new(), #{foo => <<"bar">>})
    ).

-endif.
