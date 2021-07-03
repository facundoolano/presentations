-module(calc_server3).

-behavior(gen_server).

-export([start_link/0, add/1, divide/1, get/0]).
-export([init/1, handle_cast/2, handle_call/3]).

%% API
start_link() ->
    gen_server:start_link({global, calc_server}, ?MODULE, [], []).

add(N) ->
    gen_server:cast({global, calc_server}, {add, N}).

divide(N) ->
    gen_server:cast({global, calc_server}, {divide, N}).

get() ->
    gen_server:call({global, calc_server}, get, _Timeout=1000).

%% gen_server callbacks

init([]) -> {ok, 0}.

handle_cast({add, N}, Acc) -> {noreply, Acc + N};
handle_cast({divide, N}, Acc) -> {noreply, Acc / N}.

handle_call(get, _From, Acc) -> {reply, Acc, Acc}.
