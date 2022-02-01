-module(controller_todos).

-include("../include/todow_http.hrl").

-export([
  allowed_methods/1,
  process/4
]).

-define(TEMPLATE, "pages/todos/new_or_edit.tpl").
-define(URL, todow_router:url_todos()).

allowed_methods(Context) ->
  {[?METHOD_GET, ?METHOD_POST, ?METHOD_PATCH], Context}.

process(?METHOD_GET, undefined, ?CONTENT_TYPE_HTML, Context) ->
  Id = todow_http:get_query(id, Context),
  render(Id, Context);

process(?METHOD_POST, ?CONTENT_TYPE_FORM_URLENCODED, ?CONTENT_TYPE_HTML, Context) ->
  Title = todow_http:get_query(<<"title">>, Context, <<"unknown">>),
  {<<"Your new task is ", Title/binary>>, Context};

process(?METHOD_PATCH, ?CONTENT_TYPE_FORM_URLENCODED, ?CONTENT_TYPE_HTML, Context) ->
  z_render:growl(<<"Ok"/utf8>>, Context).

render(Id, Context) ->
  Vars = [
    {resource_id, Id},
    {form_method, get_method(Id)},
    {form_url, ?URL}
  ],
  todow_controller:render(?TEMPLATE, Vars, Context).

get_method(undefined) -> ?METHOD_POST;
get_method(_Id) -> ?METHOD_PATCH.
