-module(example2).

-export([list_increase/1]).

%% Incrementar en 1 todos los elementos de la lista.
list_increase(List) ->
    lists:map(fun(N) -> N + 1 end, List).
