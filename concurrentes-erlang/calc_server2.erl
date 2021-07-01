-module(calc_server2).

-export([start_link/0, add/1, divide/1, get/0]).


%% API
start_link() ->
    %% iniciar servidor con un link al proceso actual
    %% cuando el servidor muera, este proceso recibirá una señal de exit
    Pid = spawn_link(fun() -> loop(0) end),

    %% darle un nombre global al proceso para poder mandarle mensajes
    %% sin conocer su Pid
    register(calc_server, Pid),
    Pid.

add(N) ->
    calc_server ! {add, N},
    ok.

divide(N) ->
    calc_server ! {divide, N},
    ok.

get() ->
    calc_server ! {get, self()},
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
        {get, FromPid} ->
            FromPid ! {calc_server_result, Acc},
            Acc;
        {add, N} -> Acc + N;
        {divide, N} -> Acc / N
    end,
    loop(NewAcc).
