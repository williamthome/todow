-module(todow_changeset).

-record(changeset, {
  changes = maps:new() :: map(),
  data = maps:new() :: map()
}).
-opaque changeset() :: #changeset{}.
-export_type([ changeset/0 ]).

-export([
  new/2,
  is_changeset/1,
  get_changes/1, set_changes/2,
  get_data/1, set_data/2
]).
-export([ cast/2, cast/3 ]).

%%====================================================================
%% Api
%%====================================================================

-spec new(Changes :: map(), Data :: map()) -> changeset().

new(Changes, Data) -> #changeset{
  changes = Changes,
  data = Data
}.

-spec is_changeset(Unknown :: any()) -> boolean().

is_changeset(Unknown) when is_record(Unknown, changeset) -> true;
is_changeset(_Unknown) -> false.

-spec get_changes(Changeset :: changeset()) -> map().

get_changes(undefined) -> undefined;
get_changes(#changeset{changes = Changes}) -> Changes.

-spec set_changes(Changeset :: changeset(), Changes :: map()) ->  changeset().

set_changes(Changeset, Changes) ->
  Changeset#changeset{changes = Changes}.

-spec get_data(Changeset :: changeset()) -> map().

get_data(undefined) -> undefined;
get_data(#changeset{data = Data}) -> Data.

-spec set_data(Changeset :: changeset(), Data :: map()) ->  changeset().

set_data(Changeset, Data) ->
  Changeset#changeset{data = Data}.

-spec cast(Data :: map(), Params :: map()) -> changeset().

cast(Data, Params) ->
  cast(Data, Params, []).

-spec cast(Data :: map(), Params :: map(), Permitted :: [atom()]) -> changeset().

cast(Data, Params, Permitted) ->
  maps:fold(
    fun(Key, Value, Changeset) ->
      case Permitted =:= [] orelse lists:member(Key, Permitted) of
        true ->
          NewData = maps:put(Key, Value, Changeset#changeset.data),
          NewChanges = maps:put(Key, Value, Changeset#changeset.changes),
          Changeset#changeset{
            data = NewData,
            changes = NewChanges
          };
        false -> Changeset
      end
    end,
    #changeset{
      data = Data
    },
    Params
  ).

%%====================================================================
%% Internal functions
%%====================================================================

%% nothing here
