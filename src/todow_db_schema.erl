-module(todow_db_schema).

-export([ create_tables/0 ]).
-export([ drop_tables/0 ]).
-export([ recreate_tables/0 ]).

create_tables() ->
  create_table_todos().

drop_tables() ->
  drop_table_todos().

recreate_tables() ->
  drop_tables(),
  create_tables().

create_table_todos() ->
  todow_db:query("
    CREATE TABLE IF NOT EXISTS todow.todos (
      id SERIAL NOT NULL PRIMARY KEY,
      title TEXT,
      description TEXT,
      created_at TIMESTAMP NOT NULL DEFAULT NOW(),
      updated_at TIMESTAMP NOT NULL DEFAULT NOW(),
      completed_at TIMESTAMP
    );
  ").

drop_table_todos() ->
  todow_db:query("DROP TABLE IF EXISTS todow.todos").
