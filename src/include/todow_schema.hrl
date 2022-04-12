-include("todow_field.hrl").

-define(private, {private, true}).
-define(required, {required, true}).
-define(default(Default), {default, Default}).
-define(validators(Validators), {validators, Validators}).

-define(field(Name, Type, Options),
    todow_field:new(Name, Type, maps:from_list(Options))
).

-define(field_id, todow_field:new_id()).

-define(timestamps, {timestamps, true}).

-define(schema(Name, Fields, Options),
    todow_schema_core:new(Name, Fields, maps:from_list(Options))
).
