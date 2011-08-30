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

-module(z_mqtt).
-author("Michael Connors <michael@bring42.net>").

-include("zotonic.hrl").

-export([get_username/1,
         get_password/1,
         connect/1,
         connect/2,
         connect/3,
         connect/4,
         subscribe/3,
         unsubscribe/3,
         publish/4,
         publish/5,
         disconnect/2
        ]).

get_username(Context) ->
    case m_config:get_value(?MODULE, username, undefined, Context) of
        undefined -> undefined;
        Username -> binary_to_list(Username)
    end.

get_password(Context) ->
    case m_config:get_value(?MODULE, password, undefined, Context) of
        undefined -> undefined;
        Password -> binary_to_list(Password)
    end.

connect(Context) ->
    AllowAnonymous = m_config:get_value(?MODULE, allow_anonymous, false, Context),
    Username = get_username(Context),
    Password = get_password(Context),
    case {AllowAnonymous, Username, Password} of
        {true, _, _} -> mqtt_client:connect("127.0.0.1", 1883, [{client_id, z_ids:unique()}]);
        {false, Username, undefined} -> mqtt_client:connect("127.0.0.1", 1883, [{username, Username}, {client_id, z_ids:unique()}]);
        {false, undefined, _Password} -> ?LOG("MQTT Broker username was not set.", []);
        {false, Username, Password} -> mqtt_client:connect("127.0.0.1", 1883, [{username, Username}, {password, Password}, {client_id, z_ids:unique()}])
    end.
connect(Server, _Context) ->
    mqtt_client:connect(Server).
connect(Server, Args, _Context) ->
    mqtt_client:connect(Server, 1883, Args).
connect(Server, Args, Port, _Context) ->
    mqtt_client:connect(Server, Port, Args).

subscribe(ClientPid, Topic, Context) ->
    mqtt_client:subscribe(ClientPid, Topic).
    %z_notifier:notify({mqtt_message, ClientPid, mqtt_client:get_message(ClientPid)}, Context).

unsubscribe(ClientPid, Topic, _Context) ->
    mqtt_client:subscribe(ClientPid, Topic).

publish(ClientPid, Topic, Message, _Context) ->
    mqtt_client:publish(ClientPid, Topic, Message).
publish(ClientPid, Topic, Message, Options, _Context) ->
    mqtt_client:publish(ClientPid, Topic, Message, Options).

disconnect(ClientPid, _Context) ->
    mqtt_client:disconnect(ClientPid).
