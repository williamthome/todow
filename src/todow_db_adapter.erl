-module(todow_db_adapter).

-callback equery(Query :: string()) -> any().
