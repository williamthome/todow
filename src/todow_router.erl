-module(todow_router).

-export([ url_todos/0, url_todos_new/0, url_todos_update/1, url_todos_delete/1 ]).

url_todos() -> url(todos).
url_todos_new() -> url(todos_new).
url_todos_update(Id) -> url(todos_update, [{id, Id}]).
url_todos_delete(Id) -> url(todos_delete, [{id, Id}]).

url(Dispatch, Args) -> z_dispatcher:url_for(Dispatch, Args, todow:context()).
url(Dispatch) -> url(Dispatch, []).
