%% Obtener el id del proceso actual (el de la shell)
> ParentPid = self().
<0.84.0>

%% Iniciar un nuevo proceso con spawn
> spawn(fun() ->
                %% Enviar un mensaje al proceso de la shell
                ParentPid ! {self(), hello_world}
        end).
<0.88.0>

%% Recibir (bloqueando) un mensaje con el patrón {From, Message}
> receive
      {From, Message} ->
          %% Imprimir el valor recibido por stdout
          io:format("Received: ~p from ~p \n", [Message, From])
  end.
Received: hello_world from <0.88.0>
ok
