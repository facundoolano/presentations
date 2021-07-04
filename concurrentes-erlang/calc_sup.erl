-module(calc_sup).

-export([start_calc_server/0]).

start_calc_server() ->
    spawn(fun() -> restarter() end).


%% Espera mensajes de EXIT del calc_server y lo reinicia
restarter() ->
    %% iniciar el server con un link al proceso supervisor
    ServerPid = calc_server2:start_link(),

    %% capturar los exits para recibir mensajes en vez de matar
    %% el proceso supervisor
    process_flag(trap_exit, true),

    receive
        {'EXIT', ServerPid, _} ->
            io:format("Restarting calc_server "),
            restarter()
    end.
