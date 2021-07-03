-module(calc_sup2).

-behavior(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link(?MODULE, []).

init([]) ->
    SupervisorFlags = #{
      strategy => one_for_all, %% si falla un worker reiniciar todos
      intensity => 5,          %% hasta 5 restarts
      period => 60             %% cada 60 segundos
    },

    ChildSpec = [#{
      id => calc_server,
      start => {calc_server3, start_link, []},
      restart => permanent
    },
    #{
      id => calc_loader,
      start => {calc_loader, start_link, []},
      restart => transient
     }],

    {ok, {SupervisorFlags, ChildSpec}}.
