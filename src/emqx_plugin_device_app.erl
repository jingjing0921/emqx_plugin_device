-module(emqx_plugin_device_app).

-behaviour(application).

-emqx_plugin(?MODULE).

-include("emqx_plugin_device.hrl").

-export([ start/2
        , stop/1
        ]).

start(_StartType, _StartArgs) ->
    {ok, Sup} = emqx_plugin_device_sup:start_link(),
    emqx_plugin_device:load(application:get_all_env()),
    {ok, Sup}.

stop(_State) ->
    eredis_cluster:stop_pool(?APP),
    emqx_plugin_device:unload().
