-module(wifilight_communicator).
-behavior(gen_server).

% for supervisor
-export([start/2, stop/0]).

% external api
-export([set_color/4]).

% for gen_server
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, code_change/3, terminate/2]).

-record(state, {socket}).

%% External API

start(Ip, Port) ->
    erlang:display("start communicator"),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Ip, Port], []).

stop() ->
    erlang:display("stop communicator"),
    gen_server:call(stop).

set_color(Pid, R, G, B) ->
    gen_server:call(Pid, {set_color, R, G, B}).

init([Ip, Port]) ->
    {ok, Socket} = gen_tcp:connect(Ip, Port, [binary, {active,true}]),
    {ok, #state{socket=Socket}}.

handle_call(stop, _From, State=#state{socket=Socket}) ->
    erlang:display("stop in handle_call"),
    gen_tcp:close(Socket),
    {stop, normal, ok, State};
handle_call({set_color, R, G, B}, _From, State=#state{socket=Socket}) ->
    erlang:display("set_color"),
    SetColor = [16#56, R, G, B, 16#AA],
    ok = gen_tcp:send(Socket, SetColor),
    erlang:display("done"),
    {reply, ok, State};
handle_call(_Message, _From, State) ->
    {noreply, State}.

handle_cast(_Message, State) ->
    {noreply, State}.

handle_info(_Message, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(normal, _State) ->
    ok.