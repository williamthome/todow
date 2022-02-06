-define(INSPECT(What), begin io:format("===\n[~p:~p] ~p\n===\n", [?MODULE, ?LINE, What]), What end).

-define(TEMPLATE_NOT_FOUND, "pages/not_found.tpl").
