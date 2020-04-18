-module(emqx_plugin_device).

-include("emqx_plugin_device.hrl").

-include_lib("emqx/include/emqx.hrl").

-export([ load/1
        , unload/0
        ]).

%% Client Lifecircle Hooks
-export([ on_client_connected/3
        , on_client_disconnected/4
        ]).


%% Called when the plugin application start
load(Env) ->
    emqx:hook('client.connected',    {?MODULE, on_client_connected, [Env]}),
    emqx:hook('client.disconnected', {?MODULE, on_client_disconnected, [Env]}).

%%--------------------------------------------------------------------
%% Client Lifecircle Hooks
%%--------------------------------------------------------------------

on_client_connected(ClientInfo, ConnInfo = #{username := UserName, peername := PeerName, proto_name := Protocol}, _Env) ->
	Key = "device:" ++ UserName,
	{{A1, A2, A3, A4}, Port} = PeerName,
	IP = lists:flatten(io_lib:format("~w.~w.~w.~w", [A1, A2, A3, A4])),
	{{Year, Month, Day}, {Hour, Minute, Second}} = calendar:local_time(),
	Time = lists:flatten(io_lib:format("~w-~w-~w ~w:~w:~w", [Year, Month, Day, Hour, Minute, Second])),
	Hash = ["online", "true", "ip", IP, "protocol", Protocol, "time", Time],
	Timeout = application:get_env(?APP, query_timeout),
	io:format("Timeout ~p~n", [Timeout]),
	emqx_plugin_device_redis_cli:q(["HMSET", Key | Hash], 1000).

on_client_disconnected(ClientInfo, ReasonCode, ConnInfo = #{username := UserName}, _Env) ->
	Key = "device:" ++ UserName,
	%%Timeout = application:get_env(?APP, query_timeout),
	emqx_plugin_device_redis_cli:q(["DEL", Key], 1000).

%% Called when the plugin application stop
unload() ->
    emqx:unhook('client.connected',    {?MODULE, on_client_connected}),
    emqx:unhook('client.disconnected', {?MODULE, on_client_disconnected}).
