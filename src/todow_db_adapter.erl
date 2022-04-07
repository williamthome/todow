-module(todow_db_adapter).

-include("./include/todow_db.hrl").

-callback get_connection() -> connection().

-callback equery(
    Connection :: connection(),
    Query :: query()
) -> result().

-callback transaction(
    Connection :: connection(),
    Fun :: transaction_fun()
) -> result().
