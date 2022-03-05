-module(todow_schema).

-record(schema, {
  name :: atom(),
  fields :: nonempty_list(todow_field:t())
}).
-opaque t() :: #schema{}.

-export_type([ t/0 ]).
