-module(todow_db_adapter).

-callback get_connection() -> todow_db_repo:connection().

-callback equery(
    Connection :: todow_db_repo:connection(),
    Query :: todow_db_query:query()
) -> todow:result().

-callback transaction(
    Connection :: todow_db_repo:connection(),
    Fun :: todow_db_repo:transaction_fun()
) -> todow:result().
