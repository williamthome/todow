-module(todow_router).

-export([ url_todos_new/0 ]).

url_todos_new() -> url(todos_new).

url(Dispatch, Args) -> z_dispatcher:url_for(Dispatch, Args, todow:context()).
url(Dispatch) -> url(Dispatch, []).
