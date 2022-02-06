-module(todow_http).

-export([ get_query/2, get_multiple_query/2 ]).

get_query({Param, Validations}, Context) ->
  {ok, Value} = do_get_query(Param, Context),
  MaybeInt = todow_convert_utils:maybe_to_integer(Value),
  case todow_validation:validate({Param, MaybeInt}, Validations) of
    {ok, _Key} -> {ok, Value};
    Error -> Error
  end;

get_query(Param, Context) ->
  do_get_query(Param, Context).

get_multiple_query(ParamList, Context) when is_list(ParamList) ->
  [ do_get_query(Param, Context) || Param <- ParamList ].

do_get_query({Param, Default}, Context) ->
  do_get_query(Param, Context, Default);

do_get_query(Param, Context) ->
  do_get_query(Param, Context, undefined).

do_get_query(Param, Context, Default) ->
  {ok, z_context:get_q(Param, Context, Default)}.
