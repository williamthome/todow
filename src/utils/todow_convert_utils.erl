-module(todow_convert_utils).

-type convert_error_reason() :: not_integer | not_string.
-type convert_error(Value) ::
    {Reason :: convert_error_reason(), Value}.
-type convert_error_result(Value) ::
    todow:error_result(convert_error(Value)).
-type convert_result(ExpectedType, ValueType) ::
    todow:result(ExpectedType, convert_error(ValueType)).

-export_type([
  convert_result/2
]).

-export([
  to_integer/1, to_integer/2,
  maybe_to_integer/1, maybe_to_integer/2,
  must_to_integer/1, must_to_integer/2,
  must_to_integer_mult/1, must_to_integer_mult/2,

  to_string/1, to_string/2,
  maybe_to_string/1, maybe_to_string/2,
  must_to_string/1, must_to_string/2,
  must_to_string_mult/1, must_to_string_mult/2
]).

%%------------------------------------------------------------------------------
%% @doc Try convert to integer.
%% @end
%%------------------------------------------------------------------------------
-spec to_integer(Value) -> convert_result(integer(), Value).

to_integer(Value) -> to_integer(Value, #{}).

-spec to_integer(Value, Options :: map()) -> convert_result(integer(), Value).

to_integer(Value = undefined, _Options) ->
  error_not_integer(Value);
to_integer(Value, _Options) when is_binary(Value) ->
  try
    {ok, erlang:binary_to_integer(Value)}
  catch
    _:_ -> error_not_integer(Value)
  end;
to_integer(Value, _Options) when is_list(Value) ->
  try
    {ok, erlang:list_to_integer(Value)}
  catch
    _:_ -> error_not_integer(Value)
  end;
to_integer(Value, _Options) -> error_not_integer(Value).

%%------------------------------------------------------------------------------
%% @doc If converted returns an integer, otherwise the input value.
%% @end
%%------------------------------------------------------------------------------
-spec maybe_to_integer(Value :: any()) -> any().

maybe_to_integer(Value) -> maybe_to_integer(Value, #{}).

-spec maybe_to_integer(Value :: any(), Options :: map()) -> any().

maybe_to_integer(Value, Options) ->
  case to_integer(Value, Options) of
    {ok, Integer} -> Integer;
    _ -> Value
  end.

%%------------------------------------------------------------------------------
%% @doc The converted value must be an integer, otherwise throws.
%%
%% @throws {error, bad_match}
%% @end
%%------------------------------------------------------------------------------
-spec must_to_integer(Value :: any()) -> integer().

must_to_integer(Value) -> must_to_integer(Value, #{}).

-spec must_to_integer(Value :: any(), Options :: map()) -> integer().

must_to_integer(Value, Options) ->
  {ok, Integer} = to_integer(Value, Options),
  Integer.

-spec must_to_integer_mult(List :: list()) -> list(integer()).

must_to_integer_mult(List) -> must_to_integer_mult(List, #{}).

-spec must_to_integer_mult(List :: list(), Options :: map()) -> list(integer()).

must_to_integer_mult(List, Options) ->
  [must_to_integer(Value, Options) || Value <- List].

%%------------------------------------------------------------------------------
%% @doc Try convert to string.
%% @end
%%------------------------------------------------------------------------------
-spec to_string(Value) -> todow:result(string(), Value).

to_string(Value) -> to_string(Value, #{}).

-spec to_string(Value, Options :: map()) -> todow:result(string(), Value).

to_string(Value = undefined, _Options) ->
  error_not_string(Value);
to_string(Value, _Options) when is_list(Value) ->
  try
    case io_lib:deep_latin1_char_list(Value) of
      true -> {ok, Value};
      false -> {ok, lists:concat([ must_to_string(X) || X <- Value ])}
    end
  catch
    _:_ -> error_not_string(Value)
  end;
to_string(Value, _Options) when is_integer(Value) ->
  try
    {ok, erlang:integer_to_list(Value)}
  catch
    _:_ -> error_not_string(Value)
  end;
to_string(Value, _Options) when is_atom(Value) ->
  try
    {ok, erlang:atom_to_list(Value)}
  catch
    _:_ -> error_not_string(Value)
  end;
to_string(Value, _Options) when is_binary(Value) ->
  try
    {ok, erlang:binary_to_list(Value)}
  catch
    _:_ -> error_not_string(Value)
  end;
to_string(Value, Options) when is_float(Value) ->
  try
    {ok, erlang:float_to_list(Value, float_options(Options))}
  catch
    _:_ -> error_not_string(Value)
  end;
to_string(Value, _Options) when is_tuple(Value) ->
  try
    to_string(erlang:tuple_to_list(Value))
  catch
    _:_ -> error_not_string(Value)
  end;
to_string(Value, _Options) when is_bitstring(Value) ->
  try
    {ok, erlang:bitstring_to_list(Value)}
  catch
    _:_ -> error_not_string(Value)
  end;
to_string(Value, _Options) when is_pid(Value) ->
  try
    {ok, erlang:pid_to_list(Value)}
  catch
    _:_ -> error_not_string(Value)
  end;
to_string(Value, _Options) when is_port(Value) ->
  try
    {ok, erlang:port_to_list(Value)}
  catch
    _:_ -> error_not_string(Value)
  end;
to_string(Value, _Options) -> error_not_string(Value).

%%------------------------------------------------------------------------------
%% @doc If converted returns a string, otherwise the input value.
%% @end
%%------------------------------------------------------------------------------
-spec maybe_to_string(Value :: any()) -> any().

maybe_to_string(Value) -> maybe_to_string(Value, #{}).

-spec maybe_to_string(Value :: any(), Options :: map()) -> any().

maybe_to_string(Value, Options) ->
  case to_string(Value, Options) of
    {ok, String} -> String;
    _ -> Value
  end.

%%------------------------------------------------------------------------------
%% @doc The converted value must be a string, otherwise throws.
%%
%% @throws {error, bad_match}
%% @end
%%------------------------------------------------------------------------------
-spec must_to_string(Value :: any()) -> string().

must_to_string(Value) ->
  must_to_string(Value, #{}).

-spec must_to_string(Value :: any(), Options :: map()) -> string().

must_to_string(Value, Options) ->
  {ok, String} = to_string(Value, Options),
  String.

%%------------------------------------------------------------------------------
%% @doc The converted value must be a list of strings, otherwise throws.
%%
%% @throws {error, bad_match}
%% @end
%%------------------------------------------------------------------------------
-spec must_to_string_mult(List :: list()) -> list(string()).

must_to_string_mult(List) -> must_to_string_mult(List, #{}).

-spec must_to_string_mult(List :: list(), Options :: map()) -> list(string()).

must_to_string_mult(List, Options) ->
  [must_to_string(Value, Options) || Value <- List].

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

-spec error_not_integer(Value) -> convert_error_result(Value).

error_not_integer(Value) ->
  todow:error(bad_arg, {not_integer, Value}, "Integer value expected.").

-spec error_not_string(Value) -> convert_error_result(Value).

error_not_string(Value) ->
  todow:error(bad_arg, {not_string, Value}, "String value expected.").

-spec float_options(Options :: any()) -> proplist:proplist().

float_options(#{decimals := {Precision, compact}}) -> [{decimals, Precision}, compact];
float_options(#{decimals := Precision}) -> [{decimals, Precision}];
float_options(#{scientific := Precision}) -> [{scientific, Precision}];
float_options(_) -> [].
