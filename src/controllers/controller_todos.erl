-module(controller_todos).

-include("../include/todow_http.hrl").

-export([ allowed_methods/1, process/4 ]).

-define(TEMPLATE_INDEX, "pages/todos/index.tpl").
-define(TEMPLATE_NEW_OR_EDIT, "pages/todos/new_or_edit.tpl").
-define(TEMPLATE_SHOW, "pages/todos/show.tpl").

allowed_methods(Context) ->
  {[?METHOD_GET, ?METHOD_POST, ?METHOD_PATCH], Context}.

process(?METHOD_GET, undefined, ?CONTENT_TYPE_HTML, Context) ->
  % z_mqtt:publish(
  %   [<<"model">>, mymodel, <<"get">>, <<"foo">>, <<"bar">>, <<"baz">>],
  %   #{ foo => bar },
  %   #{ qos => 0, retain => false },
  %   Context
  % ),

  Action = z_context:get(action, Context),
  render(Action, Context);

process(?METHOD_POST, ?CONTENT_TYPE_FORM_URLENCODED, ?CONTENT_TYPE_HTML, Context) ->
  {ok, Title} = todow_http:get_query(<<"title">>, Context, #{default => <<"unknown">>}),
  {<<"Your new task is ", Title/binary>>, Context};

process(?METHOD_PATCH, ?CONTENT_TYPE_FORM_URLENCODED, ?CONTENT_TYPE_HTML, Context) ->
  % z_render:growl(<<"Ok"/utf8>>, Context).
  {ok, Title} = todow_http:get_query(<<"title">>, Context, #{default => <<"unknown">>}),
  {<<"Your task updated to ", Title/binary>>, Context}.

%%====================================================================
%% Internal functions
%%====================================================================

%% @doc render

render(index = Action, Context) ->
  do_render(?TEMPLATE_INDEX, Action, Context);

render(edit = Action, Context) ->
  case todow_http:get_query(id, Context, #{validations => [validate_is_integer]}) of
    {ok, Id} ->
      FormAction = todow_router:url_todos(),
      Vars = [ {id, Id}, {form_method, ?METHOD_PATCH}, {form_action, FormAction} ],
      do_render(?TEMPLATE_NEW_OR_EDIT, Action, Context, Vars);
    _Error ->
      todow_controller:render_not_found(Context)
  end;

render(new = Action, Context) ->
  FormAction = todow_router:url_todos(),
  Vars = [ {form_method, ?METHOD_POST}, {form_action, FormAction} ],
  do_render(?TEMPLATE_NEW_OR_EDIT, Action, Context, Vars);

render(show = Action, Context) ->
  case todow_http:get_query(id, Context, #{validations => [validate_is_integer]}) of
    {ok, Id} ->
      Vars = [ {id, Id} ],
      do_render(?TEMPLATE_SHOW, Action, Context, Vars);
    _Error ->
      todow_controller:render_not_found(Context)
  end.

%% @doc do_render

do_render(Template, Action, Context) ->
  do_render(Template, Action, Context, []).

do_render(Template, Action, Context, Vars0) ->
  Vars = [ [ {action, Action} ] | Vars0 ],
  todow_controller:render(Template, Vars, Context).
