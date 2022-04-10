-module(todow_db_schema).

-export([
    setup/0, setup/1,
    recreate/0, recreate/1,
    drop/0, drop/1,

    create_schema/0, create_schema/1,
    set_default_schema/3,
    set_schema_owner/2,
    drop_schema/0, drop_schema/1,
    recreate_schema/0, recreate_schema/1,

    create_tables/0, create_tables/1,
    drop_tables/0, drop_tables/1,
    recreate_tables/0, recreate_tables/1,
    create_table_todos/0, create_table_todos/1,
    drop_table_todos/0, drop_table_todos/1
]).

-define(USER_DEFAULT, postgres).
-define(USER_ZOTONIC, zotonic).
-define(USERS, [
    ?USER_DEFAULT,
    ?USER_ZOTONIC
]).
-define(SCHEMA_DEFAULT, zotonic).
-define(SCHEMA_ZOTONIC, zotonic).
-define(USER_SCHEMAS(Schema), ["$user" | do_schema_list(Schema)]).
-define(USER_SCHEMAS_DEFAULT, ?USER_SCHEMAS(?SCHEMA_DEFAULT)).
-define(TABLE_TODOS, todos).
-define(TABLES, [
    ?TABLE_TODOS
]).

%%%=============================================================================
%%% Setup functions
%%%=============================================================================

setup() ->
    setup(?SCHEMA_DEFAULT).

setup(Schema) ->
    create_schema(Schema),
    create_tables(Schema).

%%%=============================================================================
%%% Recreate functions
%%%=============================================================================

recreate() ->
    recreate(?SCHEMA_DEFAULT).

recreate(Schema) ->
    recreate_schema(Schema),
    recreate_tables(Schema),
    set_users_default_schema(?USERS, ?TABLES, ?USER_SCHEMAS(Schema)).

%%%=============================================================================
%%% Cleanup functions
%%%=============================================================================

drop() ->
    drop(?SCHEMA_DEFAULT).

drop(Schema) ->
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
    create_schema(?SCHEMA_DEFAULT).

create_schema(Schema) ->
    todow_db:fequery("CREATE SCHEMA IF NOT EXISTS $1", [Schema]).

%%------------------------------------------------------------------------------
%% @doc Sets the defaults schemas. Order matters.
%% @end
%%------------------------------------------------------------------------------

set_default_schema(User, DbName, Schema) ->
    Schemas = do_schema_list(Schema),

    SchemasAsString = todow_db_query:not_quoted_comma_separated(Schemas),
    todow_db:fequery(
        "ALTER ROLE $1 IN DATABASE $2 SET search_path TO $3",
        [User, DbName, SchemasAsString]
    ).

%%------------------------------------------------------------------------------
%% @doc Sets the user defaults schema or schemas. Order matters.
%% @end
%%------------------------------------------------------------------------------

set_user_default_schema(User, Tables, Schema) ->
    lists:foreach(
        fun(Table) -> set_default_schema(User, Table, Schema) end,
        Tables
    ).

%%------------------------------------------------------------------------------
%% @doc Sets users defaults schema or schemas. Order matters.
%% @end
%%------------------------------------------------------------------------------

set_users_default_schema(Users, Tables, Schema) ->
    lists:foreach(
        fun(User) -> set_user_default_schema(User, Tables, Schema) end,
        Users
    ).

%%------------------------------------------------------------------------------
%% @doc Sets the schema owner.
%% @end
%%------------------------------------------------------------------------------

set_schema_owner(Schema, Owner) ->
    todow_db:fequery(
        "ALTER SCHEMA $1 OWNER to $2",
        [Schema, Owner]
    ).

%%------------------------------------------------------------------------------
%% @doc Drop schema.
%% @end
%%------------------------------------------------------------------------------

drop_schema() ->
    drop_schema(?SCHEMA_DEFAULT).

drop_schema(Schema) ->
    todow_db:fequery("DROP SCHEMA IF EXISTS $1 CASCADE", [Schema]).

%%------------------------------------------------------------------------------
%% @doc Recreate schema.
%% @end
%%------------------------------------------------------------------------------

recreate_schema() ->
    recreate_schema(?SCHEMA_DEFAULT).

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
    create_tables(?SCHEMA_DEFAULT).

create_tables(Schema) ->
    create_table_todos(Schema).

%%------------------------------------------------------------------------------
%% @doc Drop all tables.
%% @end
%%------------------------------------------------------------------------------

drop_tables() ->
    drop_tables(?SCHEMA_DEFAULT).

drop_tables(Schema) ->
    drop_table_todos(Schema).

%%------------------------------------------------------------------------------
%% @doc Recreate table todos.
%% @end
%%------------------------------------------------------------------------------

recreate_tables() ->
    recreate_tables(?SCHEMA_DEFAULT).

recreate_tables(Schema) ->
    drop_tables(Schema),
    create_tables(Schema).

%%------------------------------------------------------------------------------
%% @doc Create table todos.
%% @end
%%------------------------------------------------------------------------------

create_table_todos() ->
    create_table_todos(?SCHEMA_DEFAULT).

create_table_todos(Schema) ->
    todow_db:fequery(
        "CREATE TABLE IF NOT EXISTS $1.$2 ("
        "    id SERIAL NOT NULL PRIMARY KEY,"
        "    title TEXT NOT NULL,"
        "    description TEXT,"
        "    created_at TIMESTAMP NOT NULL DEFAULT NOW(),"
        "    updated_at TIMESTAMP NOT NULL DEFAULT NOW(),"
        "    completed_at TIMESTAMP"
        ")",
        [Schema, ?TABLE_TODOS]
    ).

%%------------------------------------------------------------------------------
%% @doc Drop table todos.
%% @end
%%------------------------------------------------------------------------------

drop_table_todos() ->
    drop_table_todos(?SCHEMA_DEFAULT).

drop_table_todos(Schema) ->
    todow_db:fequery(
        "DROP TABLE IF EXISTS $1.$2",
        [Schema, ?TABLE_TODOS]
    ).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

do_schema_list(Schema) when is_list(Schema) ->
    case io_lib:char_list(Schema) of
        true -> [Schema];
        false -> Schema
    end;
do_schema_list(Schema) ->
    [Schema].
