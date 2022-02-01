-module(todow_db).

-export([ query/1, query/2 ]).

query(Sql, Args) -> z_db:equery(Sql, Args, todow:context()).
query(Sql) -> query(Sql, []).
