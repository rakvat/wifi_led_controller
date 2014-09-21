-module(wifilight_player).
-behavior(gen_event).

% external api
-export([set_transition/5]).

% for supervisor
-export([start/0, stop/0]).

% for get_event
-export([init/1, handle_event/2, handle_call/2, 
         handle_info/2, terminate/2, code_change/3]).

-record(state, {timer}).

set_transition(Pid, R, G, B, Duration) ->
    gen_event:notify(Pid, {transition, R, G, B, Duration}).

start() ->
    erlang:display("start player"),
    {ok, Pid} = gen_event:start_link({local, ?MODULE}),
    gen_event:add_handler(Pid, wifilight_player, []),
    {ok, Pid}.

stop() ->
    erlang:display("stop event"),
    gen_event:call(stop).

init([]) ->
    {ok, []}.

handle_event({transition, R, G, B, Duration}, State) ->
    erlang:display("transition"),
    % TODO
    % ask web for current state
    % play with delays
    %timer:sleep(10000), %ms
    TimerRef = erlang:send_after(3000, wifilight_communicator, {set_color, R, G, B}),
    erlang:display("transition done"),
    {ok, State};
handle_event(_, State) ->
    {ok, State}.

handle_call(_, State) ->
    {ok, ok, State}.

handle_info(_, State) ->
    {ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.
