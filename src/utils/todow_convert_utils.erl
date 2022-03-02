-module(todow_convert_utils).

-export([
  to_integer/1,
  maybe_to_integer/1,
  must_to_integer/1, must_to_integer_mult/1,
  to_string/1,
  maybe_to_string/1,
  must_to_string/1, must_to_string_mult/1
]).

-type not_integer() :: {error, not_integer}.
-type not_string() :: {error, not_string}.

-export_type([ not_integer/0, not_string/0 ]).

-define(NOT_INTEGER_ERROR, {error, not_integer}).
-define(NOT_STRING_ERROR, {error, not_string}).

-spec to_integer(Value :: any()) -> {ok, integer()} | {error, not_integer}.

to_integer(Value) when is_binary(Value) ->
  try
    {ok, erlang:binary_to_integer(Value)}
  catch
    _:_ -> ?NOT_INTEGER_ERROR
  end;
to_integer(Value) when is_list(Value) ->
  try
    {ok, erlang:list_to_integer(Value)}
  catch
    _:_ -> ?NOT_INTEGER_ERROR
  end;
to_integer(_Value) -> ?NOT_INTEGER_ERROR.

-spec maybe_to_integer(Value :: any()) -> any().

maybe_to_integer(Value) ->
  case to_integer(Value) of
    {ok, Integer} -> Integer;
    _ -> Value
  end.

-spec must_to_integer(Value :: any()) -> integer().

must_to_integer(Value) ->
  {ok, Integer} = to_integer(Value),
  Integer.

-spec must_to_integer_mult(List :: list()) -> list(integer()).

must_to_integer_mult(List) ->
  [must_to_integer(Value) || Value <- List].

-spec to_string(Value :: any()) -> {ok, string()} | {error, not_string}.

to_string(Value) when is_list(Value) ->
  case io_lib:deep_latin1_char_list(Value) of
    true -> {ok, Value};
    false -> ?NOT_STRING_ERROR
  end;
to_string(Value) when is_integer(Value) ->
  try
    {ok, erlang:integer_to_list(Value)}
  catch
    _:_ -> ?NOT_STRING_ERROR
  end;
to_string(Value) when is_atom(Value) ->
  try
    {ok, erlang:atom_to_list(Value)}
  catch
    _:_ -> ?NOT_STRING_ERROR
  end;
to_string(Value) when is_binary(Value) ->
  try
    {ok, erlang:binary_to_list(Value)}
  catch
    _:_ -> ?NOT_STRING_ERROR
  end;
to_string(Value) when is_float(Value) ->
  try
    {ok, erlang:float_to_list(Value)}
  catch
    _:_ -> ?NOT_STRING_ERROR
  end;
to_string(Value) when is_tuple(Value) ->
  try
    {ok, erlang:tuple_to_list(Value)}
  catch
    _:_ -> ?NOT_STRING_ERROR
  end;
to_string(Value) when is_bitstring(Value) ->
  try
    {ok, erlang:bitstring_to_list(Value)}
  catch
    _:_ -> ?NOT_STRING_ERROR
  end;
to_string(Value) when is_pid(Value) ->
  try
    {ok, erlang:pid_to_list(Value)}
  catch
    _:_ -> ?NOT_STRING_ERROR
  end;
to_string(Value) when is_port(Value) ->
  try
    {ok, erlang:port_to_list(Value)}
  catch
    _:_ -> ?NOT_STRING_ERROR
  end;
to_string(_Value) -> ?NOT_STRING_ERROR.

-spec maybe_to_string(Value :: any()) -> any().

maybe_to_string(Value) ->
  case to_string(Value) of
    {ok, String} -> String;
    _ -> Value
  end.

-spec must_to_string(Value :: any()) -> string().

must_to_string(Value) ->
  {ok, String} = to_string(Value),
  String.

-spec must_to_string_mult(List :: list()) -> list(string()).

must_to_string_mult(List) ->
  [must_to_string(Value) || Value <- List].
