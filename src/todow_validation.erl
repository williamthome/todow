-module(todow_validation).

-export([
  validate_required/1, validate_required/2,
  validate_number/2,
  validate_is_integer/1, validate_is_integer/2
]).
-export([ validate/1, validate/2 ]).

%% @doc validate_required

validate_required(Value, #{})
  when Value =/= undefined
  andalso Value =/= ""
  andalso Value =/= <<>> ->
  ok;

validate_required(_, _) ->
  {error, required}.

validate_required(Value) ->
  validate_required(Value, #{}).

%% @doc validate_number

validate_number(Value, #{range := {Min, Max}})
  when Value >= Min andalso Value =< Max ->
  ok;

validate_number(_, #{range := {Min, Max}}) ->
  {error, {range, {Min, Max}}}.

%% @doc validate_is_integer

validate_is_integer(Value) when is_integer(Value) ->
  ok;

validate_is_integer(_Value) ->
  {error, not_integer}.

validate_is_integer(Value, #{}) ->
  validate_is_integer(Value).

%% @doc validate

validate({Key, Value}, Validations) ->
  case validate(Value, Validations) of
    ok -> {ok, Key};
    {error, Reason} -> {error, {Key, Reason}}
  end;

validate(Value, Validations) ->
  do_validate(Value, Validations, ok).

validate(ToValidate) when is_list(ToValidate) ->
  case lists:foldl(
    fun({{Key, Value}, Validations}, Acc) ->
      case validate({Key, Value}, Validations) of
        {ok, _Key} -> Acc;
        {error, {Key, Reason}} -> [{Key, Reason} | Acc]
      end
    end,
    [],
    ToValidate
  ) of
    [] -> #{errors => [], valid => true};
    Errors -> #{errors => Errors, valid => false}
  end.

%%====================================================================
%% Internal functions
%%====================================================================

do_validate(Value, [Function | Validations], ok)
  when is_atom(Function) ->
  do_validate(Value, [{Function, #{}} | Validations], ok);

do_validate(Value, [Function | Validations], ok)
  when is_function(Function, 2) ->
  do_validate(Value, [{Function, #{}} | Validations], ok);

do_validate(Value, [{Function, Args} | Validations], ok)
  when is_function(Function, 2) andalso is_map(Args) ->
  do_validate(Value, Validations, Function(Value, Args));

do_validate(Value, [{Function, Args} | Validations], ok)
  when is_atom(Function) andalso is_map(Args) ->
  do_validate(Value, Validations, ?MODULE:Function(Value, Args));

do_validate(Value, [{Module, Function} | Validations], ok)
  when is_atom(Module) andalso is_atom(Function) ->
  do_validate(Value, [{Module, Function, #{}} | Validations], ok);

do_validate(Value, [{Module, Function, Args} | Validations], ok)
  when is_atom(Module) andalso is_atom(Function) andalso is_map(Args) ->
  do_validate(Value, Validations, apply(Module, Function, [Value, Args]));

do_validate(_Value, _Validations, {error, Error}) ->
  {error, Error};

do_validate(_Value, [], Result) ->
  Result.
