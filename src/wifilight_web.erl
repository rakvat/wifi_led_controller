%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc Web server for wifilight.

-module(wifilight_web).
-author("Mochi Media <dev@mochimedia.com>").

-export([start/1, stop/0, loop/2]).

%% External API

start(Options) ->
    {DocRoot, Options1} = get_option(docroot, Options),
    Loop = fun (Req) ->
                   ?MODULE:loop(Req, DocRoot)
           end,
    mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).

stop() ->
    mochiweb_http:stop(?MODULE).

loop(Req, DocRoot) ->

    Communicator = whereis(wifilight_communicator),
    "/" ++ Path = Req:get(path),
    try
        case Req:get(method) of
            Method when Method =:= 'GET'; Method =:= 'HEAD' ->
                case Path of
                    "register" ->
                        Hint = "ip address",
                        {ok, HTMLOutput} = register_dtl:render([{format_hint, Hint}]),
                        Req:respond({200, [{"Content-Type", "text/html"}], HTMLOutput});
                    "red" ->
                        wifilight_communicator:set_color(Communicator, 255, 0, 0),
                        Req:respond({200, [{"Content-Type", "text/plain"}], "ok"});
                    "green" ->
                        wifilight_communicator:set_color(Communicator, 0, 255, 0),
                        Req:respond({200, [{"Content-Type", "text/plain"}], "ok"});
                    "blue" ->
                        wifilight_communicator:set_color(Communicator, 0, 0, 255),
                        Req:respond({200, [{"Content-Type", "text/plain"}], "ok"});
                    "color" ->
                        QueryStringData = Req:parse_qs(),
                        Red = list_to_integer(proplists:get_value("r", QueryStringData, "0")),
                        Green = list_to_integer(proplists:get_value("g", QueryStringData, "0")),
                        Blue = list_to_integer(proplists:get_value("b", QueryStringData, "0")),
                        wifilight_communicator:set_color(Communicator, Red, Green, Blue),
                        Req:respond({200, [{"Content-Type", "text/plain"}], "ok"});
                    "hello" ->
                        QueryStringData = Req:parse_qs(),
                        Username = proplists:get_value("username", QueryStringData, "Anonymous"),
                        Req:respond({200, [{"Content-Type", "text/plain"}],
                                            "Hello " ++ Username ++ "!\n"});
                    _ ->
                        Req:serve_file(Path, DocRoot)
                end;
            'POST' ->
                case Path of
                    _ ->
                        Req:not_found()
                end;
            _ ->
                Req:respond({501, [], []})
        end
    catch
        Type:What ->
            Report = ["web request failed",
                      {path, Path},
                      {type, Type}, {what, What},
                      {trace, erlang:get_stacktrace()}],
            error_logger:error_report(Report),
            Req:respond({500, [{"Content-Type", "text/plain"}],
                         "request failed, sorry\n"})
    end.

%% Internal API

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.

%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

you_should_write_a_test() ->
    ?assertEqual(
       "No, but I will!",
       "Have you written any tests?"),
    ok.

-endif.
