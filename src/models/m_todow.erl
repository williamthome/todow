-module(m_todow).

-include("../include/todow.hrl").
-include("../include/todow_schema.hrl").

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
    FieldsName = todow_schema:field_names(?SCHEMA),
    ?assert(
        lists:all(
            fun(Name) -> lists:member(Name, FieldsName) end,
            [id, title, created_at, updated_at]
        )
    ).

-endif.
