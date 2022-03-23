-module(todow_utils).

-include("../include/todow.hrl").

-export([ maybe_default/2, maybe_default/3, maybe_default/4 ]).

-spec maybe_default(Value :: any(), Default :: any()) -> any().

maybe_default(undefined, Default) -> Default;
maybe_default(Value, _Default) -> Value.

-spec maybe_default(Key :: any(), Value :: any(), Defaults :: map()) -> any().

maybe_default(Key, Value, Defaults) ->
  maybe_default(Key, Value, Defaults, []).

maybe_default(Key, undefined, Defaults, Args) ->
  Default = maps:get(Key, Defaults, undefined),
  maybe_default_is_function(Default, Args);
maybe_default(_Key, Value, _Defaults, _Args) -> Value.

maybe_default_is_function(Default, []) when is_function(Default, 0) ->
  Default();
maybe_default_is_function(Default, Args) when is_function(Default, 1) ->
  Default(Args);
maybe_default_is_function(Default, _Args) -> Default.

%%%=============================================================================
%%% Tests
%%%=============================================================================

-ifdef(TEST).

maybe_default_test() ->
  % maybe_default/2
  ?assertEqual(foo, maybe_default(foo, bar)),
  ?assertEqual(bar, maybe_default(undefined, bar)),
  % maybe_default/3
  ?assertEqual(bar, maybe_default(foo, undefined, #{foo => bar})),
  ?assertEqual(bar, maybe_default(foo, bar, #{foo => foo})),
  % maybe_default/3
  ?assertEqual(bar, maybe_default(foo, undefined, #{foo => fun() -> bar end})),
  ?assertEqual(bar, maybe_default(foo, bar, #{foo => fun() -> foo end})),
  % maybe_default/4
  ?assertEqual(bar, maybe_default(foo, undefined, #{foo => fun(Args) -> Args end}, bar)),
  ?assertEqual(bar, maybe_default(foo, bar, #{foo => fun(Args) -> Args end}, foo)).

-endif.
