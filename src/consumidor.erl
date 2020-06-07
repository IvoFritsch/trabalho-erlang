-module(consumidor).
-export([start/1, consomerandom/1]).

start({E, NC, Ws}) -> 
    consomerandom({E, NC, Ws}).

esperaproduto({E, NC, Ws}) -> 
    receive
        coca -> 
            io:format("Consumidor ~w -> Consumindo coca\n", [NC]),
            enviaStateConsumidor({NC, "Coca", "CONSUMINDO", Ws}),
            timer:apply_after(7000, consumidor, consomerandom, [{E, NC, Ws}]);
        fanta -> 
            io:format("Consumidor ~w -> Consumindo fanta\n", [NC]),
            enviaStateConsumidor({NC, "Fanta", "CONSUMINDO", Ws}),
            timer:apply_after(15000, consumidor, consomerandom, [{E, NC, Ws}]);
        {vazio, coca} -> 
            %io:format("Consumidor ~w -> vazio coca\n", [NC]),
            solicitaproduto(coca, {E, NC, Ws});
        {vazio, fanta} -> 
            %io:format("Consumidor ~w -> vazio fanta\n", [NC]),
            solicitaproduto(fanta, {E, NC, Ws})
    end.

solicitaproduto(coca, {E, NC, Ws}) -> 
    E ! {solicitada, coca, self()},
    esperaproduto({E, NC, Ws});

solicitaproduto(fanta, {E, NC, Ws}) -> 
    E ! {solicitada, fanta, self()},
    esperaproduto({E, NC, Ws});

solicitaproduto(1, {E, NC, Ws}) -> 
    io:format("Consumidor ~w -> Solicitando coca\n", [NC]),
    enviaStateConsumidor({NC, "Coca", "ESPERANDO", Ws}),
    solicitaproduto(coca, {E, NC, Ws});

solicitaproduto(2, {E, NC, Ws}) -> 
    io:format("Consumidor ~w -> Solicitando fanta\n", [NC]),
    enviaStateConsumidor({NC, "Fanta", "ESPERANDO", Ws}),
    solicitaproduto(fanta, {E, NC, Ws}).

consomerandom({E, NC, Ws}) ->
    solicitaproduto(rand:uniform(2), {E, NC, Ws}).

enviaStateConsumidor({NC, Produto, Acao, Ws}) ->
    Ws ! {send, io_lib:format("{ \"event\": \"CONSUMIDOR-~s\", \"nc\": ~w, \"produto\": \"~s\" }", [Acao, NC, Produto])}.

