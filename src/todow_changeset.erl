-module(todow_changeset).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-type action() :: new | update | undefined.

-record(changeset, {
    data = maps:new() :: map(),
    changes = maps:new() :: map(),
    action :: action()
}).
-opaque t() :: #changeset{}.

-export_type([t/0, action/0]).

-export([
    new/2, new/3,
    is_changeset/1,
    get_changes/1,
    set_changes/2,
    get_data/1,
    set_data/2,
    get_action/1,
    set_action/2
]).
-export([cast/3, cast/4]).

-define(is_cast_action(Action), Action =:= new orelse Action =:= update).

%%------------------------------------------------------------------------------
%% @doc Changeset constructor.
%% @end
%%------------------------------------------------------------------------------

-spec new(Data :: map(), Changes :: map()) -> {ok, t()}.

new(Data, Changes) ->
    new(Data, Changes, guess_action(Data)).

%%------------------------------------------------------------------------------
%% @doc Changeset constructor.
%% @end
%%------------------------------------------------------------------------------

-spec new(Data :: map(), Changes :: map(), Action :: action()) -> {ok, t()}.

new(Data, Changes, Action) ->
    Changeset = #changeset{
        data = Data,
        changes = Changes,
        action = Action
    },
    {ok, Changeset}.

%%------------------------------------------------------------------------------
%% @doc Check if is changeset record.
%% @end
%%------------------------------------------------------------------------------

-spec is_changeset(Unknown :: any()) -> boolean().

is_changeset(Unknown) when is_record(Unknown, changeset) -> true;
is_changeset(_Unknown) -> false.

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

-spec set_changes(Changeset :: t(), Changes :: map()) -> t().

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

-spec set_data(Changeset :: t(), Data :: map()) -> t().

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
%% @doc Cast to changeset.
%% @end
%%------------------------------------------------------------------------------

-spec cast(
    Data :: map(), Changes :: map(), ValidKeys :: list()
) -> {ok, t()}.

cast(Data, Changes, ValidKeys) -> cast(Data, Changes, ValidKeys, #{}).

%%------------------------------------------------------------------------------
%% @doc Cast to changeset.
%% @end
%%------------------------------------------------------------------------------

-spec cast(
    Data :: map(), Changes :: map(), ValidKeys :: list(), Options :: map()
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
    {ok, Changeset}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Guess action.
%% @end
%%------------------------------------------------------------------------------

-spec guess_action(Data :: map()) -> new | update.

guess_action(Data) when map_size(Data) == 0 -> new;
guess_action(Data) when is_map(Data) -> update.

%%------------------------------------------------------------------------------
%% @doc Maybe merge defaults.
%% @end
%%------------------------------------------------------------------------------

-spec maybe_merge_defaults(
    ActionOrData :: new | update, Changes :: map(), Defaults :: map()
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
    ?assertEqual(
        #changeset{data = #{foo => bar}, changes = #{foo => bar}},
        maybe_set_change(
            #changeset{data = #{foo => bar}, changes = #{foo => bar}}, foo, bar, bar
        )
    ),
    ?assertEqual(
        #changeset{data = #{foo => baz}, changes = #{foo => baz}},
        maybe_set_change(
            #changeset{data = #{foo => bar}, changes = #{foo => bar}}, foo, bar, baz
        )
    ).

-endif.
