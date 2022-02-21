-module(todow_utils).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([ maybe_default/3 ]).

-spec maybe_default(Key :: any(), Value :: any(), Defaults :: map()) -> any().

maybe_default(Key, undefined, Defaults) -> maps:get(Key, Defaults, undefined);
maybe_default(_Key, Value, _Defaults) -> Value.

%%====================================================================
%% Tests
%%====================================================================

-ifdef(TEST).

maybe_default_test() ->
  ?assertEqual(bar, maybe_default(foo, undefined, #{foo => bar})),
  ?assertEqual(bar, maybe_default(foo, bar, #{foo => foo})).

-endif.
