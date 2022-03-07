-ifdef(PROD).
-define(INSPECT(What), What).
-else.
-define(INSPECT(What),
  begin
    io:format("===\n[~p:~p] ~p\n===\n", [?MODULE, ?LINE, What]),
    What
  end
).
-endif.

-define(OK(Result), {ok, Result}).
-define(ERROR(Reason), ?INSPECT({error, Reason})).
