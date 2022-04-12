-module(todo_schema).
-behavior(todow_schema).

-export([
    schema/0,
    cast/1,
    validates/1
]).

schema() -> todow_schema_core:new(
    todo,
    [
        todow_field:new_id(),
        todow_field:new(title, binary, #{required => true})
    ]
).

cast(Changes) ->
    todow_schema:cast(?MODULE, Changes).

validates(Changes) ->
    todow_schema:validates(?MODULE, Changes).
