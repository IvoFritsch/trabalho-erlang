-module(produtor).
-export([start/1, finalizaproducaococa/1, finalizaproducaofanta/1, produz/2]).

start({E, NP, Ws}) -> 
    produzrandom({E, NP, Ws}).

produz(1, {E, NP, Ws}) -> 
    io:format("Produtor ~w -> Comecando a produzir coca\n", [NP]),
    enviaStateProdutor({NP, "Coca", Ws}),
    timer:apply_after(3500, produtor, finalizaproducaococa, [{E, NP, Ws}]);

produz(2, {E, NP, Ws}) -> 
    io:format("Produtor ~w -> Comecando a produzir fanta\n", [NP]),
    enviaStateProdutor({NP, "Fanta", Ws}),
    timer:apply_after(7500, produtor, finalizaproducaofanta, [{E, NP, Ws}]).

finalizaproducaococa({E, NP, Ws}) -> 
    io:format("Produtor ~w -> Terminada coca\n", [NP]),
    E ! {produzida, coca},
    produzrandom({E, NP, Ws}).

finalizaproducaofanta({E, NP, Ws}) -> 
    io:format("Produtor ~w -> Terminada fanta\n", [NP]),
    E ! {produzida, fanta},
    produzrandom({E, NP, Ws}).

produzrandom({E, NP, Ws}) -> 
    enviaStateProdutor({NP, "", Ws}),
    timer:apply_after(rand:uniform(1100) + 400, produtor, produz, [rand:uniform(2), {E, NP, Ws}]).

enviaStateProdutor({NP, Produto, Ws}) -> 
    Ws ! {send, io_lib:format("{ \"event\": \"PRODUTOR\", \"np\": ~w, \"produto\": \"~s\" }", [NP, Produto])}.
