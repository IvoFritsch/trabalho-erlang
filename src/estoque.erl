-module(estoque).
-export([start/0]).

start() -> 
    Ws = websocket:start(),
    spawn(produtor, start, [{self(), 1, Ws}]),
    spawn(produtor, start, [{self(), 2, Ws}]),

    spawn(consumidor, start, [{self(), 1, Ws}]),
    spawn(consumidor, start, [{self(), 2, Ws}]),
    spawn(consumidor, start, [{self(), 3, Ws}]),
    spawn(consumidor, start, [{self(), 4, Ws}]),
    recebesolicitacoes({0,0, Ws}).

recebesolicitacoes({QtdFanta, QtdCoca, Ws}) ->
    receive
        {produzida, coca} -> 
            io:format("Estoque -> Recebi coca\n"),
            io:format("--------------------------->>>>>> Fantas: ~w | Cocas ~w\n", [QtdFanta, QtdCoca + 1]),
            enviaStateEstoque({QtdFanta, QtdCoca + 1, Ws}),
            recebesolicitacoes({QtdFanta, QtdCoca + 1, Ws});
        {produzida, fanta} -> 
            io:format("Estoque -> Recebi fanta\n"),
            io:format("--------------------------->>>>>> Fantas: ~w | Cocas ~w\n", [QtdFanta + 1, QtdCoca]),
            enviaStateEstoque({QtdFanta + 1, QtdCoca, Ws}),
            recebesolicitacoes({QtdFanta + 1, QtdCoca, Ws});
        {solicitada, coca, C} -> 
            if QtdCoca > 0 ->
                io:format("Estoque -> Entregando coca\n"),
                C ! coca,
                io:format("--------------------------->>>>>> Fantas: ~w | Cocas ~w\n", [QtdFanta, QtdCoca - 1]),
                enviaStateEstoque({QtdFanta, QtdCoca - 1, Ws}),
                recebesolicitacoes({QtdFanta, QtdCoca - 1, Ws});
            QtdCoca == 0 ->
                C ! {vazio, coca},
                recebesolicitacoes({QtdFanta, QtdCoca, Ws})
            end;
        {solicitada, fanta, C} -> 
            if QtdFanta > 0 ->
                io:format("Estoque -> Entregando fanta\n"),
                C ! fanta,
                io:format("--------------------------->>>>>> Fantas: ~w | Cocas ~w\n", [QtdFanta - 1, QtdCoca]),
                enviaStateEstoque({QtdFanta - 1, QtdCoca, Ws}),
                recebesolicitacoes({QtdFanta - 1, QtdCoca, Ws});
            QtdFanta == 0 ->
                C ! {vazio, fanta},
                recebesolicitacoes({QtdFanta, QtdCoca, Ws})
            end
    end.

enviaStateEstoque({QtdFanta, QtdCoca, Ws}) -> 
    Ws ! {send, io_lib:format("{ \"event\": \"ESTOQUE\", \"QtdFanta\": ~w, \"QtdCoca\": ~w}", [QtdFanta, QtdCoca])}.