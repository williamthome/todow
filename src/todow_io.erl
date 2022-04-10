-module(todow_io).

-export([
    inspect/3,
    inspect_format/3
]).

-spec inspect(Module :: module(), Line :: integer(), What) -> What.

inspect(Module, Line, What) ->
    io:format(inspect_format(Module, Line, What)),
    What.

-spec inspect_format(
    Module :: module(),
    Line :: integer(),
    What :: any()
) -> string().

inspect_format(Module, Line, What) ->
    io_lib:format("\n===\n[~p:~p] ~p\n===\n", [Module, Line, What]).
