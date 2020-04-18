%%--------------------------------------------------------------------
%% Copyright (c) 2020 EMQ Technologies Co., Ltd. All Rights Reserved.
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
%%--------------------------------------------------------------------

-module(emqx_plugin_device_redis_cli).

-behaviour(ecpool_worker).

-include("emqx_plugin_device.hrl").

-include_lib("emqx/include/emqx.hrl").
-include_lib("emqx/include/logger.hrl").

-import(proplists, [get_value/2, get_value/3]).

-export([ connect/1
        , q/2
        ]).

%%--------------------------------------------------------------------
%% Redis Connect/Query
%%--------------------------------------------------------------------

connect(Opts) ->
	io:format("connect Opts ~p~n", [Opts]),
    Sentinel = get_value(sentinel, Opts),
    Host = case Sentinel =:= "" of
        true -> get_value(host, Opts);
        false ->
            eredis_sentinel:start_link(get_value(servers, Opts)),
            "sentinel:" ++ Sentinel
    end,
    case eredis:start_link(
                    Host,
                    get_value(port, Opts, 6379),
                    get_value(database, Opts),
                    get_value(password, Opts),
                    no_reconnect
                ) of
            {ok, Pid} ->
				io:format("Connect Redis success. ~n"),
				{ok, Pid};
            {error, Reason = {connection_error, _}} ->
                ?LOG(error, "[Redis] Can't connect to Redis server: Connection refused."),
                {error, Reason};
            {error, Reason = {authentication_error, _}} ->
                ?LOG(error, "[Redis] Can't connect to Redis server: Authentication failed."),
                {error, Reason};
            {error, Reason} ->
                ?LOG(error, "[Redis] Can't connect to Redis server: ~p", [Reason]),
                {error, Reason}
    end.

%% Redis Query.
-spec(q(string(), timeout())
      -> {ok, undefined | binary() | list()} | {error, atom() | binary()}).
q(Cmd, Timeout) ->
	io:format("Query ~p~n", [Cmd]),
    case get_value(type, application:get_env(?APP, server, [])) of
        cluster -> eredis_cluster:q(?APP, Cmd);
        _ -> ecpool:with_client(?APP, fun(C) -> eredis:q(C, Cmd, Timeout) end)
    end.

