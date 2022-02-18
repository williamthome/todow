-module(meta_schema).

-include_lib("syntax_tools/include/merl.hrl").

-export([ parse_transform/2 ]).

-record(state, {
  options,
  module,
  schema
}).

-record(function, {
  name,
  arity,
  form
}).

-record(transform, {
  functions = [] :: list(#function{})
}).

parse_transform(Forms, Options) ->
  {Forms1, State} = parse_trans:transform(
    fun do_transform/4,
    #state{ options = Options },
    Forms,
    Options
  ),

  io:format("State: ~p\n", [State]),

  Forms2 = do_state_forms(State, Forms1),

  parse_trans:revert(Forms2).

do_transform(attribute, {attribute, _Line, module, Module} = Form, _Context, State) ->
  {Form, false, State#state{ module = Module }};

do_transform(attribute, {attribute, _Line, schema, Schema} = Form, _Context, State) ->
  {Form, false, State#state{ schema = Schema }};

do_transform(_Type, Form, _Context, State) ->
  {Form, true, State}.

do_schema_functions(Table, Fields) when is_atom(Table) andalso is_list(Fields) ->
  [
    #function{
      name = fetch_table,
      arity = 0,
      form = ?Q("fetch_table() -> _@Table@.")
    },
    #function{
      name = fetch_fields,
      arity = 0,
      form = ?Q("fetch_fields() -> _@Fields@.")
    }
  ].

do_key_function(Key) when is_atom(Key) ->
  #function{
    name = Key,
    arity = 1,
    form = ?Q("'@Key@'(#{'@Key@' := Value}) -> Value.")
  }.

do_fields_functions(Fields) when is_list(Fields) ->
  lists:foldl(
    fun(#{name := Name}, Acc) -> [do_key_function(Name) | Acc] end,
    [],
    Fields
  ).

do_transform_schema(
  #transform{functions = Functions},
  #{table := Table, fields := Fields}
) ->
  SchemaFunctions = do_schema_functions(Table, Fields),
  FieldsFunctions = do_fields_functions(Fields),
  TransformedFunctions = lists:merge3(Functions, SchemaFunctions, FieldsFunctions),
  #transform{
    functions = TransformedFunctions
  }.

do_transform_forms(#transform{functions = TransformedFunctions}, Forms) ->
  {ExportForms, FunctionForms} = lists:foldl(
    fun(#function{name = Name, arity = Arity, form = Form}, {ExportFormsAcc, FunctionFormsAcc}) ->
      {parse_trans:export_function(Name, Arity, ExportFormsAcc), [Form | FunctionFormsAcc]}
    end,
    {Forms, []},
    TransformedFunctions
  ),
  lists:merge(ExportForms, FunctionForms).

do_state_forms(#state{schema = Schema}, Forms) ->
  Transform = do_transform_schema(#transform{}, Schema),
  do_transform_forms(Transform, Forms).
