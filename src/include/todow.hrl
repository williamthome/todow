-ifdef(TEST).
-define(INSPECT(What),
    begin
        ?debugFmt(todow_io:inspect_format(?MODULE, ?LINE, What)),
        What
    end
).
-else.
-define(INSPECT(What),
    begin
        io:format(todow_io:inspect_format(?MODULE, ?LINE, What)),
        What
    end
).
-endif.
