%% @author Michael Connors <michael@bring42.net>
%% @copyright 2011 Michael Connors
%% @date 2011-08-19
%% @doc Module to provide an MQTT broker and client

%% Copyright 2011 Michael Connors
%%
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

-module(mod_mqtt).
-author("Michael Connors <michael@bring42.net>").

-mod_title("MQTT Module").
-mod_description("Module for MQTT").
-mod_prio(500).

-include_lib("zotonic.hrl").

-record(state, {context, clients=[]}).

%% interface functions
-export([
    init/1
]).


init(Context) ->
    application:start(mqtt_broker),
    {ok, #state{context=z_context:new(Context)}}.

