#!/usr/bin/escript

main(_Args) ->
    Port = 5577,
    Ip = {127,0,0,1},
    RValue = 200,
    GValue = 100,
    BValue = 250,
    
    {ok, Socket} = gen_tcp:connect(Ip, Port, [binary, {active,true}]),
    SetColor = [16#56, RValue, GValue, BValue, 16#AA],
    gen_tcp:send(Socket, SetColor),
    % A = gen_tcp:recv(Socket,0),
    io:format("done~n"),
    gen_tcp:close(Socket).
