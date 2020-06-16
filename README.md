# Trabalho produtor-consumidor Erlang

## Autores

Ivo Fritsch e Lucas Land.

## Motivação e contexto

O projeto tem como objetivo estudar o problema denominado "produtor consumidor" e propor um sistema na linguagem Erlang, que implemente e simule a resolução do problema, a partir da bibliografia da disciplina **PROCESSAMENTO PARALELO - 4N - 202001**.

A escolha do Erlang como linguagem parte do estudo das peculiaridades de seu paradigma funcional e alto paralelismo, em contraste a paradigmas mais convencionais os quais estamos mais familizarizados (como os orientados a objetos ou procedurais). Nesse problema é necessária uma solução escalável e paralela, com isso foi selecionado o Erlang como linguagem para o controle e lógica principal do sistema.

## O que acontece na aplicação (tl;dr)

Ao executar a aplicação backend, são criados os seguintes processos do sistema: 1 de estoque, 2 de produtores (que produzem dois produtos, coca e fanta) e outros 4 de consumidores (que consomem esses produtos, aleatoriamente).

Ao longo da execução, os produtores produzem produtos com um tempo de produção, o qual o da coca leva 3.5s, já o da fanta leva 7,5s. O tempo de consumo desses produtos levam 7s e 15s, respectivamente. O estoque age como processo central, recebendo os produtos dos produtores e solictações de consumo dos consumidores e retornando aos consumidores conforme disponibilidade do produto.

Além disso, um processo que atua como servidor de WebSocket fica em execução, expondo os principais eventos do sistema com os dados necessários e sendo consumidos pelo frontend em React, o qual exibe os estados dos processos em um grid visual de itens.  

## Como executar

### Backend (Erlang):

1. Necessita do Erlang e da ferramenta [rebar3](https://www.rebar3.org/docs/getting-started) instalados.
2. Executar no terminal do projeto:
```
$ rebar3 shell
1> estoque:start().
```  

### Frontend (React web):

1. Necessita da ferramenta [yarn](https://yarnpkg.com/getting-started/install) instalada.
2. Executar no terminal do projeto:
```
$ cd web
$ yarn start
```
3. Acessar `localhost:3000`

## Detalhamento

### Estoque

É o ponto principal da lógica relevante do sistema. Inicia com `start`:

```erlang
start() -> 
    Ws = websocket:start(),
    spawn(produtor, start, [{self(), 1, Ws}]),
    spawn(produtor, start, [{self(), 2, Ws}]),

    spawn(consumidor, start, [{self(), 1, Ws}]),
    spawn(consumidor, start, [{self(), 2, Ws}]),
    spawn(consumidor, start, [{self(), 3, Ws}]),
    spawn(consumidor, start, [{self(), 4, Ws}]),
    recebesolicitacoes({0,0, Ws}).
```
Nesse trecho inicial, ocorre a inicialização de diferentes processos, como o do WebSocket, dos produtores e consumidores.

A função `recebesolicitacoes` roda durante toda a execução do sistema e recebe mensagens dos processos de consumidores e produtores e faz os devidos tratamentos de acordo com o caso. Usando de exemplo o recebimento e solitação de cocas:

```erlang
{produzida, coca} -> 
   io:format("Estoque -> Recebi coca\n"),
   io:format("--------------------------->>>>>> Fantas: ~w | Cocas ~w\n", [QtdFanta, QtdCoca + 1]),
   enviaStateEstoque({QtdFanta, QtdCoca + 1, Ws}),
   recebesolicitacoes({QtdFanta, QtdCoca + 1, Ws});
...
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
```

Através de `enviaStateEstoque` a WebSocket comunica a aplicação web de uma alteração no estado do estoque. Quando um produto é solicitado, uma mensagem de retorno é enviada para o pid `C`(Consumidor que solicitou o produto), de acordo com disponibilidade em estoque.

### Produtor

Os produtores executam a produção aleatória dos 2 tipos de produtos em um intervalo de tempo dinâmico, definindo por um timer:
```erlang
produzrandom({E, NP, Ws}) -> 
    enviaStateProdutor({NP, "", Ws}),
    timer:apply_after(rand:uniform(1100) + 400, produtor, produz, [rand:uniform(2), {E, NP, Ws}]).
```

Usando de exemplo a produção de coca, a qual leva 3,5 segundos fixos, de acordo com especificação do projeto:
```erlang
produz(1, {E, NP, Ws}) -> 
    io:format("Produtor ~w -> Comecando a produzir coca\n", [NP]),
    enviaStateProdutor({NP, "Coca", Ws}),
    timer:apply_after(3500, produtor, finalizaproducaococa, [{E, NP, Ws}]);
```

Quando a producão é finalizada, uma mensagem é enviada para o processo de estoque informando que o produto está pronto e o produtor reinicia sua lógica de produção:
```erlang
finalizaproducaococa({E, NP, Ws}) -> 
    io:format("Produtor ~w -> Terminada coca\n", [NP]),
    E ! {produzida, coca},
    produzrandom({E, NP, Ws}).
```

### Consumidor

Os consumidores executam o consumo alteatório dos 2 tipos de produtos, sem tempo de espera entre consumo (esses consumidores são gulosos):
```erlang
consomerandom({E, NC, Ws}) ->
    solicitaproduto(rand:uniform(2), {E, NC, Ws}).
```
A solicitação do consumidor é feita na sequência de duas funções. A primeira recebe o número gerado aleatoriamente por `consomerandom` e envia os estados para a WebSocket e o segundo envia a mensagem para o estoque:
```erlang
solicitaproduto(1, {E, NC, Ws}) -> 
    io:format("Consumidor ~w -> Solicitando coca\n", [NC]),
    enviaStateConsumidor({NC, "Coca", "ESPERANDO", Ws}),
    solicitaproduto(coca, {E, NC, Ws});
...    
solicitaproduto(coca, {E, NC, Ws}) -> 
    E ! {solicitada, coca, self()},
    esperaproduto({E, NC, Ws});    
```

Após isso é aguardada a mensagem de retorno do estoque, se recebeu ou não o produto:
```erlang
receive
  coca -> 
      io:format("Consumidor ~w -> Consumindo coca\n", [NC]),
      enviaStateConsumidor({NC, "Coca", "CONSUMINDO", Ws}),
      timer:apply_after(7000, consumidor, consomerandom, [{E, NC, Ws}]);
  ...
  {vazio, coca} -> 
      solicitaproduto(coca, {E, NC, Ws});
end.
```
Recebendo o produto, o mesmo é consumindo por um tempo fixo (dobro do tempo de produção do tipo de produto), de acordo com especificações do projeto. Caso não receba o produto, o consumidor reinicia o processo de solicitação.

### Interface Web

Para melhor visualização é oferecido uma interface web feita em React.

Foi utilizada a biblioteca [Mochiweb](https://github.com/basho/mochiweb) para inicializar um servidor de WebSocket pelo backend em Erlang:

```erlang
start() ->
   spawn(
      fun () ->
        application:start(sasl),
        Broadcaster = start_link(),
        esperaMensagens(Broadcaster)
      end).
...
esperaMensagens(Broadcaster) ->
   receive
      stop -> ok;
      {send, State} -> 
         Broadcaster ! {broadcast, self(), State},
         esperaMensagens(Broadcaster)
   end.
```
O `Broadcaster` envia as mensagens de `enviaStateEstoque`, `enviaStateProdutor` e `enviaStateConsumidor`. Essas mensagens são consumidas pelo app React no seguinte trecho:

```javascript
let ws = new WebSocket('ws://localhost:8080')
ws.onmessage = (m) => {
   if(!m.data.startsWith('{')) return
   const payload = JSON.parse(m.data)
   switch(payload.event){
      case "ESTOQUE":
       setEstoque(e => (
         {
           ...e, 
           ...payload
         }
       ))
       break
      case "PRODUTOR":
       setProdutores(p => (
         {
           ...p, 
           [payload.np]: payload.produto
         }
       ))
       break 
      ...
```
O payload da mensagem é recebido e seus dados utlizados na renderização dos componentes designados, como por exemplo dos consumidores:

```javascript
{Object.keys(consumidores).map(nc => 
   <React.Fragment key={nc}>
      <Grid item xs={12}>
         <Paper style={resolveStyleProduto(consumidores[nc].produto, consumidores[nc].acao)}>
            <div style={{...}}>
               <h2 style={{...}}>Consumidor {nc}</h2>
               {consumidores[nc].acao}: {consumidores[nc].produto}
            </div>
         </Paper>
      </Grid>
   </React.Fragment>
)}
```
## Considerações Finais

Ao longo do desenvolvimento do projeto experienciamos os principais pontos em trabalhar com o Erlang:
- Necessidade de pensar de maneira diferente ao desenvolver a arquitetura do projeto, deixando de lado os "vícios" da orientação em objeto e abraçando conceitos de programação funcional.
- Não precisamos nos preocupar com erros de mensagem entre os processos. Relacionamos com o título da tese de Joe Armstong, designer da linguagem: "Tornar sistemas distribuídos confiáveis ​​na presença de erros de software.".
- Podemos adicionar mais *n* processos de consumidores e produtores apenas alterando o início do estoque, sem necessitar demais ajustes no código. Isso mostra a escalabilidade desse sistema e como é importante na aplicação em sistemas reais de troca de mensagens, por exemplo.


## Licença

[![License: CC BY-NC 4.0](https://img.shields.io/badge/License-CC%20BY--NC%204.0-lightgrey.svg)](https://creativecommons.org/licenses/by-nc/4.0/)

Pode usar a vontade, mas não esquece de nos referenciar.
