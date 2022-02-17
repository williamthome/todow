-module(meta_schema).

-include_lib("syntax_tools/include/merl.hrl").

-export([ parse_transform/2 ]).

-record(state, {
  options,
  module,
  schema
}).

parse_transform(Forms, Options) ->
  {Forms1, State} = parse_trans:transform(
    fun do_transform/4,
    #state{ options = Options },
    Forms,
    Options
  ),

  Forms2 = parse_trans:do_insert_forms(
    below,
    do_forms(State),
    Forms1,
    State
  ),

  io:format("State: ~p\n", [State]),

  parse_trans:revert(Forms2).

do_transform(attribute, {attribute, _Line, module, Module} = Form, _Context, State) ->
  {Form, false, State#state{ module = Module }};

do_transform(attribute, {attribute, _Line, schema, Schema} = Form, _Context, State) ->
  {Form, false, State#state{ schema = Schema }};

do_transform(_Type, Form, _Context, State) ->
  {Form, true, State}.

do_forms(#state{
  schema = #{
    table := TableName,
    fields := Fields
  }
}) when is_atom(TableName) andalso is_list(Fields) -> [
    ?Q("-export([fetch_table/0, fetch_fields/0])."),
    ?Q("fetch_table() -> _@TableName@."),
    ?Q("fetch_fields() -> _@Fields@.")
  ].
