-module(todow_changeset).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-type data() :: map().
-type changes() :: map().
-type action() :: new | update | undefined.
-type errors() :: map().
-type valid() :: boolean().

-define(DEFAULT_DATA, maps:new()).
-define(DEFAULT_CHANGES, maps:new()).
-define(DEFAULT_ACTION, undefined).
-define(DEFAULT_ERRORS, maps:new()).
-define(DEFAULT_VALID, false).
-define(DEFAULTS, #{
    data => ?DEFAULT_DATA,
    changes => ?DEFAULT_CHANGES,
    action => ?DEFAULT_ACTION,
    errors => ?DEFAULT_ERRORS,
    valid => ?DEFAULT_VALID
}).

-define(is_cast_action(Action), Action =:= new orelse Action =:= update).

-record(changeset, {
    data = ?DEFAULT_DATA :: data(),
    changes = ?DEFAULT_CHANGES :: changes(),
    action = ?DEFAULT_ACTION :: action(),
    errors = ?DEFAULT_ERRORS :: errors(),
    valid = ?DEFAULT_VALID :: valid()
}).
-opaque t() :: #changeset{}.

-export_type([
    t/0,
    data/0,
    changes/0,
    action/0,
    errors/0,
    valid/0
]).

-export([
    new/0, new/2, new/3, new/4,
    is_changeset/1,
    get_changes/1, set_changes/2,
    get_data/1, set_data/2,
    get_action/1, set_action/2,
    get_errors/1, with_errors/1, put_error/3,
    is_valid/1, set_valid/1
]).
-export([
    changes_without_errors/1,
    cast/3, cast/4
]).

%%------------------------------------------------------------------------------
%% @doc Changeset constructor with defaults.
%% @end
%%------------------------------------------------------------------------------

-spec new() -> t().

new() -> #changeset{}.

%%------------------------------------------------------------------------------
%% @doc Changeset constructor.
%% @end
%%------------------------------------------------------------------------------

-spec new(Data :: data(), Changes :: changes()) -> {ok, t()}.

new(Data, Changes) -> new(Data, Changes, guess_action(Data)).

%%------------------------------------------------------------------------------
%% @doc Changeset constructor.
%% @end
%%------------------------------------------------------------------------------

-spec new(Data :: data(), Changes :: changes(), Action :: action()) -> {ok, t()}.

new(Data, Changes, Action) -> new(Data, Changes, Action, maps:new()).

%%------------------------------------------------------------------------------
%% @doc Changeset constructor.
%% @end
%%------------------------------------------------------------------------------

-spec new(
    Data :: data(), Changes :: changes(), Action :: action(), Errors :: errors()
) -> {ok, t()}.

new(Data, Changes, Action, Errors) ->
    Changeset = #changeset{
        data = Data,
        changes = Changes,
        action = Action,
        errors = Errors,
        valid = map_size(Errors) =:= 0
    },
    {ok, Changeset}.

%%------------------------------------------------------------------------------
%% @doc Check if is changeset record.
%% @end
%%------------------------------------------------------------------------------

-spec is_changeset(Unknown :: any()) -> boolean().

is_changeset(Unknown) -> is_record(Unknown, changeset).

%%------------------------------------------------------------------------------
%% @doc Get changeset changes.
%% @end
%%------------------------------------------------------------------------------

-spec get_changes(Changeset :: t()) -> map().

get_changes(undefined) -> undefined;
get_changes(#changeset{changes = Changes}) -> Changes.

%%------------------------------------------------------------------------------
%% @doc Set changeset changes.
%% @end
%%------------------------------------------------------------------------------

-spec set_changes(Changeset :: t(), Changes :: changes()) -> t().

set_changes(Changeset, Changes) -> Changeset#changeset{changes = Changes}.

%%------------------------------------------------------------------------------
%% @doc Get changeset data.
%% @end
%%------------------------------------------------------------------------------

-spec get_data(Changeset :: t()) -> map().

get_data(undefined) -> undefined;
get_data(#changeset{data = Data}) -> Data.

%%------------------------------------------------------------------------------
%% @doc Set changeset data.
%% @end
%%------------------------------------------------------------------------------

-spec set_data(Changeset :: t(), Data :: data()) -> t().

set_data(Changeset, Data) -> Changeset#changeset{data = Data}.

%%------------------------------------------------------------------------------
%% @doc Get changeset action.
%% @end
%%------------------------------------------------------------------------------

-spec get_action(Changeset :: t()) -> action().

get_action(undefined) -> undefined;
get_action(#changeset{action = Action}) -> Action.

%%------------------------------------------------------------------------------
%% @doc Set changeset action.
%% @end
%%------------------------------------------------------------------------------

-spec set_action(Changeset :: t(), Action :: action()) -> t().

set_action(Changeset, Action) -> Changeset#changeset{action = Action}.

%%------------------------------------------------------------------------------
%% @doc Get changeset errors.
%% @end
%%------------------------------------------------------------------------------

-spec get_errors(Changeset :: t()) -> map().

get_errors(undefined) -> undefined;
get_errors(#changeset{errors = Errors}) -> Errors.

%%------------------------------------------------------------------------------
%% @doc Returns true if changeset errors have values.
%% @end
%%------------------------------------------------------------------------------

-spec with_errors(Payload :: t() | errors()) -> boolean().

with_errors(#changeset{errors = Errors}) -> with_errors(Errors);

with_errors(Errors) -> map_size(Errors) =/= 0.

%%------------------------------------------------------------------------------
%% @doc Returns true if changeset is valid, otherwise false.
%% @end
%%------------------------------------------------------------------------------

-spec is_valid(Changeset :: t()) -> valid().

is_valid(undefined) -> false;
is_valid(#changeset{valid = Valid}) -> Valid.

%%------------------------------------------------------------------------------
%% @doc Put an error for the given key into the changeset.
%% @end
%%------------------------------------------------------------------------------

-spec put_error(Changeset :: t(), Key :: any(), Error :: any()) -> t().

put_error(Changeset, Key, Error) ->
    Errors = maps:put(Key, Error, get_errors(Changeset)),
    Changeset#changeset{errors = Errors}.

%%------------------------------------------------------------------------------
%% @doc Set changeset action.
%% @end
%%------------------------------------------------------------------------------

-spec set_valid(Changeset :: t()) -> t().

set_valid(Changeset) -> set_valid(Changeset, not with_errors(Changeset)).

-spec set_valid(Changeset :: t(), Valid :: valid()) -> t().

set_valid(Changeset, Valid) -> Changeset#changeset{valid = Valid}.

%%------------------------------------------------------------------------------
%% @doc Return only changes who keys are not present in changeset errors.
%% @end
%%------------------------------------------------------------------------------

-spec changes_without_errors(Changeset :: t()) -> map().

changes_without_errors(#changeset{errors = Errors, changes = Changes}) ->
    maps:without(maps:keys(Errors), Changes).

%%------------------------------------------------------------------------------
%% @doc Cast to changeset.
%% @end
%%------------------------------------------------------------------------------

-spec cast(
    Data :: data(), Changes :: changes(), ValidKeys :: list()
) -> {ok, t()}.

cast(Data, Changes, ValidKeys) -> cast(Data, Changes, ValidKeys, #{}).

%%------------------------------------------------------------------------------
%% @doc Cast to changeset.
%% @end
%%------------------------------------------------------------------------------

-spec cast(
    Data :: data(), Changes :: changes(), ValidKeys :: list(), Options :: map()
) -> {ok, t()}.

cast(Data, Changes, ValidKeys, Options) when
    is_map(Changes) andalso
        is_list(ValidKeys) andalso
        is_map(Options)
->
    Action = guess_action(Data),
    Defaults = maps:get(defaults, Options, maps:new()),
    Changeset = maps:fold(
        fun(Key, Value, ChangesetAcc) ->
            case lists:member(Key, ValidKeys) of
                true ->
                    OldValue = maps:get(Key, Data, undefined),
                    NewValue = todow_utils:maybe_default(Key, Value, Defaults),
                    maybe_set_change(ChangesetAcc, Key, OldValue, NewValue);
                false ->
                    ChangesetAcc
            end
        end,
        #changeset{
            action = Action,
            data = Data
        },
        maybe_merge_defaults(Action, Changes, Defaults)
    ),
    {ok, set_valid(Changeset)}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Guess action.
%% @end
%%------------------------------------------------------------------------------

-spec guess_action(Data :: data()) -> new | update.

guess_action(Data) when map_size(Data) == 0 -> new;
guess_action(Data) when is_map(Data) -> update.

%%------------------------------------------------------------------------------
%% @doc Maybe merge defaults.
%% @end
%%------------------------------------------------------------------------------

-spec maybe_merge_defaults(
    Action :: new | update, Changes :: changes(), Defaults :: map()
) -> map().

maybe_merge_defaults(new, Changes, Defaults) -> maps:merge(Defaults, Changes);
maybe_merge_defaults(update, Changes, _Defaults) -> Changes.

%%------------------------------------------------------------------------------
%% @doc Set changeset change.
%% @end
%%------------------------------------------------------------------------------

-spec set_change(
    Changeset :: t(), Key :: any(), Value :: any()
) -> t().

set_change(Changeset, Key, Value) ->
    Changes = maps:put(Key, Value, get_changes(Changeset)),
    Data = maps:put(Key, Value, get_data(Changeset)),
    Changeset#changeset{
        changes = Changes,
        data = Data
    }.

%%------------------------------------------------------------------------------
%% @doc Maybe set changeset change.
%% @end
%%------------------------------------------------------------------------------

-spec maybe_set_change(
    Changeset :: t(), Key :: any(), OldValue :: any(), NewValue :: any()
) -> t().

maybe_set_change(Changeset, _Key, OldValue, OldValue) -> Changeset;
maybe_set_change(Changeset, Key, _OldValue, NewValue) -> set_change(Changeset, Key, NewValue).

%%%=============================================================================
%%% Tests
%%%=============================================================================

-ifdef(TEST).

guess_action_test() ->
    ?assertEqual(new, guess_action(#{})),
    ?assertEqual(update, guess_action(#{foo => bar})).

maybe_merge_defaults_test() ->
    ?assertEqual(#{foo => bar}, maybe_merge_defaults(new, #{}, #{foo => bar})),
    ?assertEqual(#{foo => bar}, maybe_merge_defaults(update, #{foo => bar}, #{})).

set_change_test() ->
    ?assertEqual(
        #changeset{data = #{foo => bar}, changes = #{foo => bar}},
        set_change(#changeset{}, foo, bar)
    ).

maybe_set_change_test() ->
    Change = #changeset{data = #{foo => bar}, changes = #{foo => bar}},
    ?assertEqual(
        #changeset{data = #{foo => bar}, changes = #{foo => bar}},
        maybe_set_change(Change, foo, bar, bar)
    ),
    ?assertEqual(
        #changeset{data = #{foo => baz}, changes = #{foo => baz}},
        maybe_set_change(Change, foo, bar, baz)
    ).

put_error_test() ->
    ?assertEqual(
        #changeset{errors = #{foo => bar}},
        put_error(#changeset{}, foo, bar)
    ).

changes_without_errors_test() ->
    ?assertEqual(
        #{foo => bar},
        changes_without_errors(#changeset{
            changes = #{foo => bar, bar => baz},
            errors = #{bar => baz}
        })
    ).

-endif.
