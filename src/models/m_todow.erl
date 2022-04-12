-module(m_todow).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("../include/todow_schema.hrl").

% TODO: Transform schema into a behavior
-define(SCHEMA, ?schema(
    todow,
    [
        ?field_id,
        ?field(title, ?binary, [?required])
    ],
    [?timestamps]
)).

%%%=============================================================================
%%% Tests
%%%=============================================================================

-ifdef(TEST).

schema_test() ->
    FieldsName = todow_schema_core:field_names(?SCHEMA),
    ?assert(
        lists:all(
            fun(Name) -> lists:member(Name, FieldsName) end,
            [id, title, created_at, updated_at]
        )
    ).

-endif.
