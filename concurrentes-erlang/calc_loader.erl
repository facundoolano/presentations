-module(calc_loader).

-export([start_link/0]).

start_link() ->
    Pid = spawn_link(fun() -> run_calculations() end),
    {ok, Pid}.


run_calculations() ->
    %% Suponer que esto se carga desde un archivo
    Operations = [{add, 1}, {add, 10}, {add, -1}, {divide, 5}],

    lists:foreach(fun ({add, N}) -> calc_server3:add(N);
                      ({divide, N}) ->calc_server3:divide(N)
                  end, Operations).
