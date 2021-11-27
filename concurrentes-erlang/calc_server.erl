-module(calc_server).

-export([start/0, add/2, divide/2, get/1]).


%% API
start() -> spawn(fun() -> loop(0) end).

add(ServerPid, N) ->
    ServerPid ! {add, N},
    ok.

divide(ServerPid, N) ->
    ServerPid ! {divide, N},
    ok.

get(ServerPid) ->
    ServerPid ! {get, self()},
    receive
        {calc_server_result, Value} ->
            Value
    after 1000 ->
            timeout
    end.

%% Server

loop(Acc) ->
    NewAcc =
        receive
            {get, ClientPid} ->
                ClientPid ! {calc_server_result, Acc},
                Acc;
            {add, N} -> Acc + N;
            {divide, N} -> Acc / N
        end,
    loop(NewAcc).
