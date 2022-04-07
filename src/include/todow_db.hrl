-define(DEFAULT_SCHEMA, public).
-define(DEFAULT_COLUMN, id).
% TODO: id transform function
-define(DEFAULT_TRANSFORM,
    fun
        (Value) when is_integer(Value) -> Value;
        (undefined) -> undefined;
        ([]) -> [];
        (Value) when is_list(Value) orelse is_binary(Value) ->
            todow_convert_utils:must_to_integer(Value)
    end
).
-define(DEFAULT_OPTIONS, #{
    schema => ?DEFAULT_SCHEMA,
    returning => #{
        column => ?DEFAULT_COLUMN,
        transform => ?DEFAULT_TRANSFORM
    }
}).

-type table() :: atom().
-type schema() :: atom().
-type column() :: atom().
-type adapter() :: atom().
-type id() :: pos_integer().
-type transform() :: fun((1) -> any()).
-type returning() ::
    column()
    | #{column => column(), transform => transform()}.
-type options() :: #{
    schema => schema(),
    returning => returning()
}.
-type result(Type) :: {ok, Type} | {error, any()}.
-type result() :: result(any()).
-type query() :: string().
-type query_param() :: any().
-type query_params() :: list(query_param()).
-type value() :: any().
-type columns() :: list(column()).
-type values() :: list(value()).
-type columns_and_values_tuple() :: {columns(), values()}.
-type columns_and_values_proplist() :: list({column(), value()}).
-type not_quoted(Type) :: {not_quote, Type}.
-type not_quoted() :: not_quoted(any()).
-type not_quoted_string() :: not_quoted(string()).
-type maybe_quoted() :: not_quoted() | null | string().
-type payload() ::
    map()
    | columns_and_values_tuple()
    | columns_and_values_proplist().
-type connection() :: any().
-type transaction_fun() :: fun((connection()) -> result()).
