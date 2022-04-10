%% @author
%% @copyright 2022
%% Generated on 2022-01-31
%% @doc This site was based on the 'empty' skeleton.

%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(todow).
-author("William Fank Thomé <williamthome@hotmail.com>").

-mod_title("todow").
-mod_description("A todo app in erlang.").
-mod_prio(10).
-mod_schema(1).

-include_lib("zotonic_core/include/zotonic.hrl").

-type result_ok(Type) :: {ok, Type}.
% TODO: Change bad_arg to bad_request
-type error_code() ::
    bad_arg
    | unauthorized
    | forbidden
    | internal_server_error.
-type error_msg() :: string().
-type result_error(Error) ::
    {error, #{
        code => error_code(),
        error => Error,
        msg => error_msg()
    }}.
-type result(OkType, ErrorType) ::
    result_ok(OkType) | result_error(ErrorType).

-export_type([
    result_ok/1,
    error_code/0,
    error_msg/0,
    result_error/1,
    result/2
]).

% Result functions
-export([
    ok/1,
    error/3
]).

% Zotonic support functions
-export([
    context/0,
    sudo_context/0,

    manage_schema/2,
    manage_data/2,

    observe_acl_is_allowed/2,

    'mqtt:test/#'/2,
    event/2
]).

%%%=============================================================================
%%% Result functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Gen ok result.
%% @end
%%------------------------------------------------------------------------------
-spec ok(Result) -> result_ok(Result).

ok(Result) -> {ok, Result}.

%%------------------------------------------------------------------------------
%% @doc Gen error result.
%% @end
%%------------------------------------------------------------------------------
-spec error(
    Code :: error_code(),
    Error,
    Msg :: error_msg()
) -> result_error(Error).

error(Code, Error, Msg) ->
    {error, #{
        code => Code,
        error => Error,
        msg => Msg
    }}.

%%%=============================================================================
%%% Zotonic support functions
%%%=============================================================================

context() -> z:c(todow).

sudo_context() -> z_acl:sudo(context()).

manage_schema(install, _Context) ->
    todow_db_schema:setup(),
    ok.

manage_data(_Version, _Context) -> ok.

observe_acl_is_allowed(
    #acl_is_allowed{object = #acl_mqtt{topic = _Topic}},
    _Context
) ->
    %% Allow anonymous access on this topic
    true;
observe_acl_is_allowed(#acl_is_allowed{}, _Context) ->
    undefined.

'mqtt:test/#'(Message, Context) ->
    io:format(
        "mqtt:test on site ~p received ~p\n",
        [z_context:site(Context), Message]
    ),
    ok.

event(#postback{message = world}, Context) ->
    z_mqtt:publish(<<"public/hello">>, <<>>, Context),
    ?DEBUG("Check the browser console o/"),
    Context.
