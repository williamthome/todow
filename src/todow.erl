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

-export([context/0, sudo_context/0]).
-export([manage_schema/2, manage_data/2]).
-export([observe_acl_is_allowed/2]).

-export([
    'mqtt:test/#'/2,
    event/2
]).

%%%=============================================================================
%%% Support functions
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
