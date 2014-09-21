-module(wifilight_communicator).
-behavior(gen_server).

% external api
-export([set_color/4]).

% for supervisor
-export([start/2, stop/0]).

% for gen_server
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, code_change/3, terminate/2]).

-record(state, {socket, r, g, b}).


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
    {ok, #state{socket=Socket, r=0, g=0, b=0}}.

handle_call(stop, _From, State=#state{socket=Socket}) ->
    erlang:display("stop in handle_call"),
    gen_tcp:close(Socket),
    {stop, normal, ok, State};
handle_call({set_color, R, G, B}, _From, State=#state{socket=Socket}) ->
    erlang:display("set_color"),
    SetColor = [16#56, R, G, B, 16#AA],
    ok = gen_tcp:send(Socket, SetColor),
    NewState = State#state{r=R, g=G, b=B},
    {reply, ok, NewState};
handle_call({get_color}, _From, State=#state{r=R, g=G, b=B}) ->
    {reply, {R, G, B}, State};
handle_call(_Message, _From, State) ->
    {noreply, State}.

handle_cast(_Message, State) ->
    {noreply, State}.

% e.g. sent from player
handle_info(Message, State) ->
    {reply, ok, NewState} = handle_call(Message, self(), State),
    {noreply, NewState}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.
