-module(todow_db_schema).

-export([
  setup/0, setup/1, cleanup/0, cleanup/1,
  create_schema/0, create_schema/1,
  drop_schema/0, drop_schema/1,
  recreate_schema/0, recreate_schema/1,
  create_tables/0, create_tables/1,
  drop_tables/0, drop_tables/1,
  recreate_tables/0, recreate_tables/1,

  create_table_todos/0, create_table_todos/1,
  drop_table_todos/0, drop_table_todos/1
]).

-define(schema, public).

%%%=============================================================================
%%% Setup functions
%%%=============================================================================

setup() ->
  setup(?schema).

setup(Schema) ->
  recreate_schema(Schema),
  recreate_tables(Schema).

%%%=============================================================================
%%% Cleanup functions
%%%=============================================================================

cleanup() ->
  cleanup(?schema).

cleanup(Schema) ->
  drop_tables(Schema),
  drop_schema(Schema).

%%%=============================================================================
%%% Schema functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Create schema.
%% @end
%%------------------------------------------------------------------------------

create_schema() ->
  create_schema(?schema).

create_schema(Schema) ->
  todow_db:equery("CREATE SCHEMA IF NOT EXISTS $1", [Schema]).

%%------------------------------------------------------------------------------
%% @doc Drop schema.
%% @end
%%------------------------------------------------------------------------------

drop_schema() ->
  drop_schema(?schema).

drop_schema(Schema) ->
  todow_db:equery("DROP SCHEMA IF EXISTS $1 CASCADE", [Schema]).

%%------------------------------------------------------------------------------
%% @doc Recreate schema.
%% @end
%%------------------------------------------------------------------------------

recreate_schema() ->
  recreate_schema(?schema).

recreate_schema(Schema) ->
  drop_schema(Schema),
  create_schema(Schema).

%%%=============================================================================
%%% Table functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Create tables.
%% @end
%%------------------------------------------------------------------------------

create_tables() ->
  create_tables(?schema).

create_tables(Schema) ->
  create_table_todos(Schema).

%%------------------------------------------------------------------------------
%% @doc Drop all tables.
%% @end
%%------------------------------------------------------------------------------

drop_tables() ->
  drop_tables(?schema).

drop_tables(Schema) ->
  drop_table_todos(Schema).

%%------------------------------------------------------------------------------
%% @doc Recreate table todos.
%% @end
%%------------------------------------------------------------------------------

recreate_tables() ->
  recreate_tables(?schema).

recreate_tables(Schema) ->
  drop_tables(Schema),
  create_tables(Schema).

%%------------------------------------------------------------------------------
%% @doc Create table todos.
%% @end
%%------------------------------------------------------------------------------

create_table_todos() ->
  create_table_todos(?schema).

create_table_todos(Schema) ->
  todow_db:equery(
    "
      CREATE TABLE IF NOT EXISTS $1.todos (
        id SERIAL NOT NULL PRIMARY KEY,
        title TEXT NOT NULL,
        description TEXT,
        created_at TIMESTAMP NOT NULL DEFAULT NOW(),
        updated_at TIMESTAMP NOT NULL DEFAULT NOW(),
        completed_at TIMESTAMP
      );
    ",
    [Schema]
  ).

%%------------------------------------------------------------------------------
%% @doc Drop table todos.
%% @end
%%------------------------------------------------------------------------------

drop_table_todos() ->
  drop_table_todos(?schema).

drop_table_todos(Schema) ->
  todow_db:equery("DROP TABLE IF EXISTS $1.todos", [Schema]).
