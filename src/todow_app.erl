-module(todow_app).
-behavior(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    io:format("Todow starting\n"),
    todow_sup:start_link().

stop(_State) -> ok.
