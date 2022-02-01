-module(controller_todos).

-include("../include/todow_http.hrl").

-export([
  allowed_methods/1,
  process/4
]).

-define(TEMPLATE, "todos/new.tpl").
-define(URL, todow_router:url_todos_new()).

allowed_methods(Context) ->
  {[?METHOD_GET, ?METHOD_POST], Context}.

process(?METHOD_GET, undefined, ?CONTENT_TYPE_HTML, Context) ->
  Vars = [ {form_url, ?URL} ],
  z_template:render_to_iolist(?TEMPLATE, Vars, Context);

process(?METHOD_POST, ?CONTENT_TYPE_FORM_URLENCODED, ?CONTENT_TYPE_HTML, Context) ->
  Title = z_context:get_q(<<"title">>, Context, <<"unknown">>),
  {<<"Your new task is ", Title/binary>>, Context}.
