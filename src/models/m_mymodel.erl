-module(m_mymodel).

-include_lib("zotonic_core/include/zotonic.hrl").

-ifdef(ZOTONIC_VERSION).
-behaviour(zotonic_model).
-endif.

-include("../include/todow_http.hrl").

-export([
  m_get/3,
  m_post/3
]).

-spec m_get(list(), zotonic_model:opt_msg(), z:context()) -> zotonic_model:return().
m_get([ <<"foo">>, <<"bar">> | Rest ], Msg, _Context) ->
  io:format("Hello, World!\n~p\n", [Msg]),

  Response = #{ baz => 1234 },
  {ok, {Response, Rest}}.

-spec m_post(list(), zotonic_model:opt_msg(), z:context()) -> zotonic_model:return().
m_post([ <<"foo">> ], Msg, _Context) ->
  io:format("My todo!\n~p\n", [Msg]),

  case Msg of
    #{payload := #{<<"title">> := Title}} when is_binary(Title) andalso Title =/= <<>> ->
      Response = #{ bar => baz },
      {ok, Response};
    _ ->
      Message = <<"Some fields are missing"/utf8>>,
      {error, Message}
  end.
