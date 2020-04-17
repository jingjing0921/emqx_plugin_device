-module(emqx_plugin_device_app).

-behaviour(application).

-emqx_plugin(?MODULE).

-export([ start/2
        , stop/1
        ]).

start(_StartType, _StartArgs) ->
	io:format("emqx_plugin_device start"),
    {ok, Sup} = emqx_plugin_device_sup:start_link(),
    emqx_plugin_device:load(application:get_all_env()),
    {ok, Sup}.

stop(_State) ->
	io:format("emqx_plugin_device stop"),
    emqx_plugin_device:unload().

