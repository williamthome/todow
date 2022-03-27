-module(todow_db_adapter).

-callback equery(Query :: string()) -> any().

-callback process_result(
    Result :: any(), Options :: todow_db:options()
) -> todow_db:result().
