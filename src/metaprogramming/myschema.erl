-module(myschema).
-include("./meta_schema.hrl").
-export([ my_function/0 ]).

-schema(#{
  table => foo,
  fields => [
    #{
      name => id,
      type => integer,
      primary_key => pk__foo__id
    },
    #{
      name => bar,
      type => boolean,
      required => true,
      default => true
    },
    #{
      name => baz,
      type => string,
      unique => unique_baz
    }
  ],
  timestamps => true
}).

my_function() -> ok.
