-module(calc_server).

-export([start/0, get/1, add/2, divide/2]).


%% API
start() -> spawn(fun() -> loop(0) end).

get(ServerPid) ->
    ServerPid ! {get, self()},
    receive
        {calc_server_result, Value} ->
            Value
    end.

add(ServerPid, N) ->
    ServerPid ! {add, N},
    ok.

divide(ServerPid, N) ->
    ServerPid ! {divide, N},
    ok.


%% Server

loop(Acc) ->
    NewAcc =
    receive
        {get, FromPid} ->
            FromPid ! {calc_server_result, Acc},
            Acc;
        {add, N} -> Acc + N;
        {divide, N} -> Acc / N
    end,
    loop(NewAcc).
