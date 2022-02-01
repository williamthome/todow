-module(todow_controller).

-export([ render/2, render/3 ]).

render(FileName, Vars, Context) ->
  z_template:render_to_iolist(FileName, Vars, Context).

render(FileName, Context) ->
  render(FileName, [], Context).
