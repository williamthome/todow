-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-ifdef(PROD).
-define(INSPECT(What), What).
-else.
-ifdef(TEST).
-define(INSPECT(What),
    begin
        ?debugFmt("\n===\n[~p:~p] ~p\n===\n", [?MODULE, ?LINE, What]),
        What
    end
).
-else.
-define(INSPECT(What),
    begin
        io:format("\n===\n[~p:~p] ~p\n===\n", [?MODULE, ?LINE, What]),
        What
    end
).
-endif.
-endif.

-define(OK(Result), {ok, Result}).
-define(ERROR(Reason), ?INSPECT({error, Reason})).
