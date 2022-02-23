-module(todow_controller).

-include("./include/todow.hrl").

-export([ render/2, render/3, render_not_found/1 ]).

render(FileName, Vars, Context) ->
  Html = z_template:render(FileName, Vars, Context),
  z_context:output(Html, Context).

render(FileName, Context) ->
  render(FileName, [], Context).

render_not_found(Context) ->
  render(?TEMPLATE_NOT_FOUND, Context).
