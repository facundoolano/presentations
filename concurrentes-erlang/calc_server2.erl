-module(calc_server2).

-export([start_link/0, add/1, divide/1, get/0]).

%% API
start_link() ->
    Pid = spawn_link(fun() -> loop(0) end),
    register(calc_server, Pid),
    Pid.

add(N) -> send({add, N}).

divide(N) -> send({divide, N}).

get() ->
    send({get, self()}),
    receive
        {calc_server_result, Value} ->
            Value
    after 1000 ->
            timeout
    end.

send(Message) ->
    ServerPid = whereis(calc_server),
    ServerPid ! Message,
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
