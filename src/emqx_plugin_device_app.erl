-module(emqx_plugin_device_app).

-behaviour(application).

-emqx_plugin(?MODULE).

-include("emqx_plugin_device.hrl").

-export([ start/2
        , stop/1
        ]).

start(_StartType, _StartArgs) ->
    {ok, Sup} = emqx_plugin_device_sup:start_link(),
	{ok, Timeout} = application:get_env(?APP, query_timeout),
	[{query_timeout, Timeout} | _] = application:get_all_env(emqx_plugin_device),
	io:format("~s~n", [Timeout]),
	
    emqx_plugin_device:load(Timeout),
    {ok, Sup}.

stop(_State) ->
	eredis_cluster:stop_pool(?APP),
    emqx_plugin_device:unload().
