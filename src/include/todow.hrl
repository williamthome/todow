-type todow_ok(Type) :: {ok, Type}.
-type todow_error_code() ::
    bad_arg
    | unauthorized
    | forbidden
    | internal_server_error.
-type todow_error_msg() :: string().
-type todow_error(What) ::
    {error, #{
        code => todow_error_code(),
        what => What,
        msg => todow_error_msg()
    }}.
-type todow_result(OkType, WhatErrorType) ::
    todow_ok(OkType) | todow_error(WhatErrorType).
-type todow_result() :: todow_result(any(), any()).

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
-ifdef(TEST).
-define(ERROR(Code, What, Msg),
    {error, #{
        code => Code,
        what => What,
        msg => Msg
    }
}).
-else.
-define(ERROR(Code, What, Msg), ?INSPECT(
    {error, #{
        code => Code,
        what => What,
        msg => Msg
    }
})).
-endif.
