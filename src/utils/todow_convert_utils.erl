-module(todow_convert_utils).

-export([
  maybe_to_integer/1, to_integer/1
]).

maybe_to_integer(Value) when is_binary(Value) ->
  try
    erlang:binary_to_integer(Value)
  catch
    _:_ -> Value
  end;
maybe_to_integer(Value) when is_list(Value) ->
  try
    erlang:list_to_integer(Value)
  catch
    _:_ -> Value
  end;
maybe_to_integer(Value) -> Value.

to_integer(Value) ->
  case maybe_to_integer(Value) of
    Int when is_integer(Int) -> {ok, Int};
    _ -> {error, not_integer}
  end.
