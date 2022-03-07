%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2022, williamthome
%%% @doc Field for schema module.
%%%
%%% @author William Fank Thomé
%%% @end
%%%-----------------------------------------------------------------------------
-module(todow_field).

-import(todow_validation, [
  validates_required/1
]).

-type type() :: integer | binary | date | boolean.

-record(field, {
  type :: type(),
  name :: atom(),
  private = true :: boolean(),
  required = false :: boolean(),
  default = undefined :: any(),
  validations = [] :: list(todow_validation:validation())
}).
-opaque t() :: #field{}.

-export_type([ t/0, type/0 ]).
