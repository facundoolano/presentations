-module(example1).

-export([list_increase/1]).

%% Incrementar en 1 todos los elementos de la lista.
list_increase(List) ->
    list_increase(List, []).

list_increase([N | Rest], Result) ->
    list_increase(Rest, [N + 1 | Result]);

list_increase([], Result) ->
    lists:reverse(Result).
