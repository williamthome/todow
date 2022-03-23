-define(METHOD_GET, <<"GET">>).
-define(METHOD_POST, <<"POST">>).
-define(METHOD_PUT, <<"PUT">>).
-define(METHOD_PATCH, <<"PATCH">>).
-define(METHOD_DELETE, <<"DELETE">>).

-define(CONTENT_TYPE_HTML, {<<"text">>, <<"html">>, []}).
-define(CONTENT_TYPE_FORM_URLENCODED,
  {<<"application">>, <<"x-www-form-urlencoded">>, []}
).
