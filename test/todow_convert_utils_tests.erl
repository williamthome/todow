-module(todow_convert_utils_tests).

-import(todow_convert_utils, [
  to_string/1, to_string/2,
  must_to_string/1, must_to_string/2
]).

-include_lib("eunit/include/eunit.hrl").

to_string_test_() ->
  [
    {
      "String be a string",
      ?_assertEqual({ok, "foo"}, to_string("foo"))
    },
    {
      "List convert to string",
      ?_assertEqual({ok, "foo"}, to_string([f, "o", <<"o">>]))
    },
    {
      "Integer convert to string",
      ?_assertEqual({ok, "1"}, to_string(1))
    },
    {
      "Atom convert to string",
      ?_assertEqual({ok, "foo"}, to_string(foo))
    },
    {
      "Binary convert to string",
      ?_assertEqual({ok, "foo"}, to_string(<<"foo">>))
    },
    {
      "Binary utf8 convert to string",
      ?_assertEqual({ok, "foo"}, to_string(<<"foo"/utf8>>))
    },
    {
      "Float with decimal precision convert to string",
      ?_assertEqual({ok, "1.10"}, to_string(1.10, #{decimals => 2}))
    },
    {
      "Tuple convert to string",
      ?_assertEqual({ok, "foo"}, to_string({f, "o", <<"o">>}))
    }
  ].

must_to_string_test_() ->
  [
    {
      "String must be a string",
      ?_assertEqual("foo", must_to_string("foo"))
    },
    {
      "List must convert to string",
      ?_assertEqual({ok, "foo"}, to_string([f, "o", <<"o">>]))
    },
    {
      "Integer must convert to string",
      ?_assertEqual("1", must_to_string(1))
    },
    {
      "Atom must convert to string",
      ?_assertEqual("foo", must_to_string(foo))
    },
    {
      "Binary must convert to string",
      ?_assertEqual("foo", must_to_string(<<"foo">>))
    },
    {
      "Binary utf8 must convert to string",
      ?_assertEqual("foo", must_to_string(<<"foo"/utf8>>))
    },
    {
      "Float with decimal precision must convert to string",
      ?_assertEqual("1.10", must_to_string(1.10, #{decimals => 2}))
    },
    {
      "Tuple must convert to string",
      ?_assertEqual("foo", must_to_string({f, "o", <<"o">>}))
    }
  ].
