-module(todow_http).

-export([ get_query/2, get_query/3 ]).

get_query(Param, Context, Default) -> z_context:get_q(Param, Context, Default).
get_query(Param, Context) -> get_query(Param, Context, undefined).
