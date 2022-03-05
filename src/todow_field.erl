-module(todow_field).

-type type() :: integer | binary | date | boolean.

-record(field, {
  type :: type(),
  name :: atom(),
  permitted = true :: boolean(),
  validations = [] :: list(),
  required = false :: boolean(),
  default = undefined :: any()
}).
-opaque t() :: #field{}.

-export_type([ t/0, type/0 ]).
