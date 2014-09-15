%% @author Mochi Media <dev@mochimedia.com>
%% @copyright wifilight Mochi Media <dev@mochimedia.com>

%% @doc Callbacks for the wifilight application.

-module(wifilight_app).
-author("Mochi Media <dev@mochimedia.com>").

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for wifilight.
start(_Type, _StartArgs) ->
    wifilight_deps:ensure(),
    wifilight_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for wifilight.
stop(_State) ->
    ok.
