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
-author("").

-mod_title("todow zotonic site").
-mod_description("An empty Zotonic site, to base your site on.").
-mod_prio(10).
-mod_schema(1).

-include_lib("zotonic_core/include/zotonic.hrl").

-export([ context/0 ]).
-export([ manage_schema/2 ]).

%%====================================================================
%% support functions go here
%%====================================================================

context() -> z:c(todow).

manage_schema(_Version, _Context) ->
    todow_db_schema:create_tables(),

    #datamodel{
        resources = [],
        media = [],
        edges = []
    }.
