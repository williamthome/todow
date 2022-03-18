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
    new/2,
    name/1,
    fields/1
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

%%------------------------------------------------------------------------------
%% @doc Get schema name.
%% @end
%%------------------------------------------------------------------------------

-spec name(Schema :: t()) -> name().

name(#schema{name = Name}) -> Name.

%%------------------------------------------------------------------------------
%% @doc Get schema fields.
%% @end
%%------------------------------------------------------------------------------

-spec fields(Schema :: t()) -> fields().

fields(#schema{fields = Fields}) -> Fields.

%%%=============================================================================
%%% Tests
%%%=============================================================================

-ifdef(TEST).

new_test() ->
    ?assertEqual(new(foo, []), #schema{name = foo, fields = []}).

-endif.
