-module(controller_todos).

-export([
  allowed_methods/1,
  process/4
]).

-define(HTML_CONTENT_TYPE, {<<"text">>,<<"html">>,[]}).
-define(FORM_URLENCODED_CONTENT_TYPE, {<<"application">>,<<"x-www-form-urlencoded">>,[]}).

-define(URL, todow_router:url_todos_new()).

allowed_methods(Context) ->
  {[<<"GET">>, <<"POST">>], Context}.

process(<<"GET">>, undefined, ?HTML_CONTENT_TYPE, Context) ->
  Vars = [ {form_url, ?URL} ],
  z_template:render_to_iolist("todos/new.tpl", Vars, Context);

process(<<"POST">>, ?FORM_URLENCODED_CONTENT_TYPE, ?HTML_CONTENT_TYPE, Context) ->
  Title = z_context:get_q(<<"title">>, Context, <<"unknown">>),
  {<<"Your new task is ", Title/binary>>, Context}.
