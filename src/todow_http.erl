-module(todow_http).

-export([ get_query/2, get_query/3, get_multiple_query/2 ]).

% TODO: Get query without zotonic context and test

get_query(Param, Context) ->
  get_query(Param, Context, #{}).

get_query(Param, Context, Options) ->
  case do_get_query(Param, Context) of
    {ok, Value} ->
      MaybeDefault = todow_utils:maybe_default(default, Value, Options),
      MaybeInt = todow_convert_utils:maybe_to_integer(MaybeDefault),
      Validations = maps:get(validations, Options, []),
      case todow_validation:validate({Param, MaybeInt}, Validations) of
        {ok, Param} -> {ok, MaybeInt};
        Error -> Error
      end;
    Error -> Error
  end.

get_multiple_query(ParamList, Context) when is_list(ParamList) ->
  [ do_get_query(Param, Context) || Param <- ParamList ].

do_get_query({Param, Default}, Context) ->
  do_get_query(Param, Context, Default);

do_get_query(Param, Context) ->
  do_get_query(Param, Context, undefined).

do_get_query(Param, Context, Default) when is_binary(Param) ->
  {ok, z_context:get_q(Param, Context, Default)};

do_get_query(_Param, _Context, _Default) ->
  {error, bad_args}.
