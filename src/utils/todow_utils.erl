-module(todow_utils).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-type prop() :: map() | proplists:proplist().

-export([
  maybe_default/2, maybe_default/3, maybe_default/4,
  get/2, get/3,
  get_else/2, get_else/3,
  wrap_get/1, wrap_get/2,
  remove/2,
  merge_to_list/2,
  factory/3, factory/4
]).

% TODO: Rename maybe_default to case_else, if_else or something like that
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

-spec get(Key :: any(), From :: prop()) -> any().

get(Key, From) -> get(Key, From, undefined).

-spec get(
    Key :: any(), From :: prop(), Default :: any()
) -> any().

get(Key, Map, Default) when is_map(Map) ->
    maybe_default(maps:get(Key, Map, Default), Default);
get(Key, Proplist, Default) when is_list(Proplist) ->
    maybe_default(proplists:get_value(Key, Proplist, Default), Default).

-spec get_else(
    Key :: any(), A :: prop(), B :: any()
) -> any().

get_else(Key, A, B) -> get_else(Key, [A, B]).

-spec get_else(Key :: any(), From :: any()) -> any().

get_else(_Key, []) ->
    undefined;
get_else(Key, [undefined | B]) ->
    get_else(Key, B);
get_else(Key, [A | B]) when is_map(A) orelse is_list(A) ->
    case get(Key, A) of
        undefined -> get_else(Key, B);
        Found -> Found
    end;
get_else(_Key, Else) ->
    Else.

-spec wrap_get(Key :: any()) -> fun((Map :: map()) -> any()).

wrap_get(Key) -> wrap_get(Key, undefined).

-spec wrap_get(Key :: any(), Default :: any()) -> fun((Map :: map()) -> any()).

wrap_get(Key, Default) -> fun(From) -> get(Key, From, Default) end.

-spec remove(Key :: any(), Prop :: prop()) -> prop().

remove(Key, Map) when is_map(Map) ->
    maps:remove(Key, Map);
remove(Key, Proplist) when is_list(Proplist) ->
    proplists:delete(Key, Proplist).

-spec merge_to_list(A :: any, B :: any) -> list().

merge_to_list(A, B) when is_list(A) andalso is_list(B) ->
    A ++ B;
merge_to_list(A, B) when is_list(B) ->
    [A] ++ B;
merge_to_list(A, B) when is_list(A) ->
    A ++ [B];
merge_to_list(A, B) ->
    [B, A].

-spec factory(
    Args :: prop(),
    Defaults :: prop(),
    MergeFun :: fun((A :: prop(), B :: prop()) -> prop())
) -> prop().

factory(Args, Defaults, MergeFun) ->
    factory(Args, Defaults, merge, MergeFun).

-spec factory(
    Args :: prop(),
    Defaults :: prop(),
    MergeKey :: any(),
    MergeFun :: fun((A :: prop(), B :: prop()) -> prop())
) -> prop().

factory(Args, Defaults, MergeKey, MergeFun) ->
    ToMerge =
        case todow_utils:get(MergeKey, Args, []) of
            X when is_list(X) -> X;
            X -> [X]
        end,
    ToMerge1 = lists:foldl(MergeFun, Defaults, ToMerge),
    Args1 = remove(MergeKey, Args),
    MergeFun(Args1, ToMerge1).

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

get_test() ->
    ?assertEqual(bar, get(foo, #{foo => bar})),
    ?assertEqual(undefined, get(foo, #{bar => baz})),
    ?assertEqual(bar, get(foo, #{foo => undefined}, bar)).

get_else_test() ->
    ?assertEqual(bar, get_else(foo, #{foo => bar}, #{foo => baz})),
    ?assertEqual(baz, get_else(foo, #{foo => undefined}, #{foo => baz})),
    ?assertEqual(undefined, get_else(bar, #{foo => any}, #{foo => any})).

wrap_get_test() ->
    Get = wrap_get(foo),
    GetDef = wrap_get(foo, bar),
    ?assertEqual(bar, Get(#{foo => bar})),
    ?assertEqual(undefined, Get(#{bar => any})),
    ?assertEqual(bar, GetDef(#{foo => undefined})).

remove_test() ->
    ?assertEqual(#{}, remove(foo, #{foo => bar})),
    ?assertEqual(#{}, remove(foo, #{})),
    ?assertEqual([], remove(foo, [{foo, bar}])),
    ?assertEqual([], remove(foo, [])).

merge_to_list_test() ->
    ?assertEqual([], merge_to_list([], [])),
    ?assertEqual([foo], merge_to_list([foo], [])),
    ?assertEqual([foo], merge_to_list([], [foo])),
    ?assertEqual([foo, bar], merge_to_list([foo], [bar])),
    ?assertEqual([foo, bar], merge_to_list(foo, [bar])),
    ?assertEqual([foo, bar], merge_to_list([foo], bar)).

factory_test() ->
    Expected = #{foo => foo, bar => bar, baz => baz},
    Args = #{bar => bar, merge => #{baz => baz}},
    Defaults = #{foo => foo},
    MergeKey = merge,
    MergeFun = fun maps:merge/2,
    ?assertEqual(Expected, factory(Args, Defaults, MergeKey, MergeFun)).

-endif.
