-module(example3).

-export([list_increase/1]).

%% Incrementar en 1 todos los elementos de la lista.
list_increase(List) ->
    [N + 1 || N <- List].
