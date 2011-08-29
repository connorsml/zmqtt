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
    init/1, terminate/2
]).

%% @spec terminate(Reason, State) -> void()
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
terminate(_Reason, State) ->
    application:stop(mqtt_broker),
    ok.

init(Context) ->
    AllowAnonymous = z_convert:to_bool(m_config:get_value(?MODULE, allow_anonymous, false, Context)),
    Username = z_mqtt:get_username(Context),
    Password = z_mqtt:get_password(Context),
    application:load(mqtt_broker),
    application:set_env(mqtt_broker, allow_anonymous, AllowAnonymous),
    application:set_env(mqtt_broker, username, Username),
    application:set_env(mqtt_broker, password, Password),
    case {AllowAnonymous, Username} of
        {false, undefined} ->
            z_session_manager:broadcast(#broadcast{type="error", message="The MQTT broker does not allow anonymous login, but there is no username configured.", title="MQTT", stay=true}, Context);
        _ ->
            application:start(mqtt_broker),
            z_session_manager:broadcast(#broadcast{type="notice", message="The MQTT broker is running.", title="MQTT", stay=false}, Context)
    end,
    {ok, #state{context=z_context:new(Context)}}.

