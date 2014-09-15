%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc wifilight.

-module(wifilight).
-author("Mochi Media <dev@mochimedia.com>").
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.


%% @spec start() -> ok
%% @doc Start the wifilight server.
start() ->
    wifilight_deps:ensure(),
    ensure_started(crypto),
    application:start(wifilight).


%% @spec stop() -> ok
%% @doc Stop the wifilight server.
stop() ->
    application:stop(wifilight).
