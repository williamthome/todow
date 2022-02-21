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
-opaque changeset() :: #changeset{}.

-export_type([ changeset/0, action/0 ]).

-export([
  new/2, new/3,
  is_changeset/1,
  get_changes/1, set_changes/2,
  get_data/1, set_data/2,
  get_action/1, set_action/2
]).
-export([ cast/3, cast/4 ]).

-define(is_cast_action(Action), Action =:= new orelse Action =:= update).

%%====================================================================
%% Api
%%====================================================================

-spec new(Data :: map(), Changes :: map()) -> {ok, changeset()}.

new(Data, Changes) ->
  new(Data, Changes, guess_action(Data)).

-spec new(Data :: map(), Changes :: map(), Action :: action()) -> {ok, changeset()}.

new(Data, Changes, Action) ->
  Changeset = #changeset{
    data = Data,
    changes = Changes,
    action = Action
  },
  {ok, Changeset}.

-spec is_changeset(Unknown :: any()) -> boolean().

is_changeset(Unknown) when is_record(Unknown, changeset) -> true;
is_changeset(_Unknown) -> false.

-spec get_changes(Changeset :: changeset()) -> map().

get_changes(undefined) -> undefined;
get_changes(#changeset{changes = Changes}) -> Changes.

-spec set_changes(Changeset :: changeset(), Changes :: map()) ->  changeset().

set_changes(Changeset, Changes) -> Changeset#changeset{changes = Changes}.

-spec get_data(Changeset :: changeset()) -> map().

get_data(undefined) -> undefined;
get_data(#changeset{data = Data}) -> Data.

-spec set_data(Changeset :: changeset(), Data :: map()) ->  changeset().

set_data(Changeset, Data) -> Changeset#changeset{data = Data}.

-spec get_action(Changeset :: changeset()) -> action().

get_action(undefined) -> undefined;
get_action(#changeset{action = Action}) -> Action.

-spec set_action(Changeset :: changeset(), Action :: action()) ->  changeset().

set_action(Changeset, Action) -> Changeset#changeset{action = Action}.

-spec cast(Data :: map(), Changes :: map(), ValidKeys :: list()) -> changeset().

cast(Data, Changes, ValidKeys) -> cast(Data, Changes, ValidKeys, #{}).

-spec cast(
  Data :: map(), Changes :: map(), ValidKeys :: list(), Options :: map()
) -> changeset().

cast(Data, Changes, ValidKeys, Options)
  when is_map(Changes)
  andalso is_list(ValidKeys)
  andalso is_map(Options) ->
    Action = guess_action(Data),
    Defaults = maps:get(defaults, Options, maps:new()),
    Changeset = maps:fold(
      fun(Key, Value, ChangesetAcc) ->
        case lists:member(Key, ValidKeys) of
          true ->
            OldValue = maps:get(Key, Data, undefined),
            NewValue = maybe_default(Key, Value, Defaults),
            maybe_set_change(ChangesetAcc, Key, OldValue, NewValue);
          false -> ChangesetAcc
        end
      end,
      #changeset{
        action = Action,
        data = Data
      },
      maybe_merge_defaults(Action, Changes, Defaults)
    ),
    {ok, Changeset}.

%%====================================================================
%% Internal functions
%%====================================================================

guess_action(Data) when map_size(Data) == 0 -> new;
guess_action(_Data) -> update.

maybe_merge_defaults(new, Changes, Defaults) -> maps:merge(Defaults, Changes);
maybe_merge_defaults(update, Changes, _Defaults) -> Changes;
maybe_merge_defaults(Data, Changes, Defaults) ->
  maybe_merge_defaults(guess_action(Data), Changes, Defaults).

maybe_default(Key, undefined, Defaults) -> maps:get(Key, Defaults, undefined);
maybe_default(_Key, Value, _Defaults) -> Value.

set_change(Changeset, Key, Value) ->
  Changes = maps:put(Key, Value, get_changes(Changeset)),
  Data = maps:put(Key, Value, get_data(Changeset)),
  Changeset#changeset{
    changes = Changes,
    data = Data
  }.

maybe_set_change(Changeset, _Key, NewValue, NewValue) -> Changeset;
maybe_set_change(Changeset, Key, _OldValue, NewValue) ->
  set_change(Changeset, Key, NewValue).

%%====================================================================
%% Tests
%%====================================================================

-ifdef(TEST).

guess_action_test() ->
  ?assertEqual(new, guess_action(#{})),
  ?assertEqual(update, guess_action(#{foo => bar})).

maybe_merge_defaults_test() ->
  ?assertEqual(#{foo => bar}, maybe_merge_defaults(new, #{}, #{foo => bar})),
  ?assertEqual(#{foo => bar}, maybe_merge_defaults(#{}, #{}, #{foo => bar})),
  ?assertEqual(#{foo => bar}, maybe_merge_defaults(update, #{foo => bar}, #{})),
  ?assertEqual(#{foo => bar}, maybe_merge_defaults(#{foo => foo}, #{foo => bar}, #{})).

-endif.
