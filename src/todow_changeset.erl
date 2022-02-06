-module(todow_changeset).

-export([ cast/2, cast/3 ]).

cast(Data, Params) ->
  cast(Data, Params, []).

cast(Data, Params, Permitted) ->
  maps:fold(
    fun(Key, Value, Acc) ->
      case Permitted =:= [] orelse lists:member(Key, Permitted) of
        true -> maps:put(Key, Value, Acc);
        false -> Acc
      end
    end,
    Data,
    Params
  ).
