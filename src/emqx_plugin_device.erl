-module(emqx_plugin_device).

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

on_client_connected(ClientInfo = #{clientid := ClientId}, ConnInfo = #{username := UserName, peername := PeerName, proto_name := Protocol}, _Env) ->
    io:format("emqx_plugin_device Client(~s) connected, ClientInfo:~n~p~n, ConnInfo:~n~p~n",
              [ClientId, ClientInfo, ConnInfo]),
	Key = "device:" ++ ClientId,
	{{A1, A2, A3, A4}, Port} = PeerName,
	IP = lists:flatten(io_lib:format("~w.~w.~w.~w",[A1, A2, A3, A4])),
	Hash = ["online", "true", "ip", IP, "protocol", Protocol],
	emqx_plugin_device_redis_cli:q(["HMSET", Key | Hash], 1000).

on_client_disconnected(ClientInfo = #{clientid := ClientId}, ReasonCode, ConnInfo, _Env) ->
    io:format("emqx_plugin_device Client(~s) disconnected due to ~p, ClientInfo:~n~p~n, ConnInfo:~n~p~n",
              [ClientId, ReasonCode, ClientInfo, ConnInfo]),
	Key = "device:" ++ ClientId,
	emqx_plugin_device_redis_cli:q(["del", Key], 1000).

%% Called when the plugin application stop
unload() ->
    emqx:unhook('client.connected',    {?MODULE, on_client_connected}),
    emqx:unhook('client.disconnected', {?MODULE, on_client_disconnected}).
