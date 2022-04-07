-module(todow_db_adapter).

-callback get_connection() -> todow_db:connection().

-callback equery(Query :: string()) -> todow_db:result().

-callback transaction(
    Connection :: todow_db:connection(), Fun :: todow_db:transaction_fun()
) -> todow_db:result().
