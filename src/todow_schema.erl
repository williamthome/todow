%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2022, williamthome
%%% @doc Schema for db module.
%%%
%%% @author William Fank Thomé
%%% @end
%%%-----------------------------------------------------------------------------
-module(todow_schema).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-type name() :: atom().
-type fields() :: nonempty_list(todow_field:t()).

-record(schema, {
    name :: name(),
    fields :: fields()
}).
-opaque t() :: #schema{}.

-export_type([t/0]).
-export([
    new/2
]).

%%------------------------------------------------------------------------------
%% @doc Schema constructor.
%% @end
%%------------------------------------------------------------------------------

-spec new(Name :: name(), Fields :: fields()) -> t().

new(Name, Fields) ->
    #schema{
        name = Name,
        fields = Fields
    }.

%%%=============================================================================
%%% Tests
%%%=============================================================================

-ifdef(TEST).

new_test() ->
    ?assertEqual(new(foo, []), #schema{name = foo, fields = []}).

-endif.
