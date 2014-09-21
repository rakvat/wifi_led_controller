-module(wifilight_player).
-behavior(gen_event).

% external api
-export([set_transition/6]).

% for supervisor
-export([start/0, stop/0]).

% for get_event
-export([init/1, handle_event/2, handle_call/2, 
         handle_info/2, terminate/2, code_change/3]).

-record(state, {active, timer, startTime, 
                startR, startG, startB, 
                goalR, goalG, goalB,
                duration, interval}).

set_transition(Pid, R, G, B, Duration, Interval) ->
    gen_event:notify(Pid, {transition, R, G, B, Duration, Interval}).

start() ->
    erlang:display("start player"),
    {ok, Pid} = gen_event:start_link({local, ?MODULE}),
    gen_event:add_handler(Pid, wifilight_player, []),
    {ok, Pid}.

stop() ->
    erlang:display("stop event"),
    gen_event:call(stop).

next_step(State=#state{interval=Interval, 
                       duration=Duration,
                       startTime=StartTime,
                       startR=SR, startG=SG, startB=SB,
                       goalR=GR, goalG=GG, goalB=GB}) ->
    Now = erlang:now(),
    TimePast = timer:now_diff(Now, StartTime)/1000.0,
    Return = 
        if TimePast < Duration ->
            T = 1.0 - (Duration - TimePast)/Duration,
            R = erlang:round(SR + (GR-SR) * T),
            G = erlang:round(SG + (GG-SG) * T),
            B = erlang:round(SB + (GB-SB) * T),
            wifilight_communicator ! {set_color, R, G, B},
            TimerRef = erlang:send_after(Interval, self(), next_step),
            NewState = State#state{timer=TimerRef},
            {ok, NewState};
        true ->
            wifilight_communicator ! {set_color, GR, GG, GB},
            NewState = State#state{active=false},
            {ok, NewState}
        end,
    Return.

init([]) ->
    {ok, #state{active=false}}.

handle_event({transition, R, G, B, Duration, Interval}, State) ->
    erlang:display("transition"),
    % ask for current state
    {StartR, StartG, StartB} = 
        gen_server:call(wifilight_communicator, {get_color}),
    TimerRef = erlang:send_after(Interval, self(), next_step),
    NewState = State#state{timer=TimerRef,
                 active=true, startTime=erlang:now(), 
                 startR=StartR, startG=StartG, startB=StartB,
                 goalR=R, goalG=G, goalB=B,
                 duration=Duration, interval=Interval},
    {ok, NewState};
handle_event(_, State) ->
    {ok, State}.

handle_call(_, State) ->
    {ok, ok, State}.

handle_info(next_step,State) ->
    {ok, NewState} = next_step(State),
    {ok, NewState};
handle_info(_, State) ->
    {ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.
