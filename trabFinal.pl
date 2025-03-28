% Salva a quantidade de produtos diferentes há no estoque
qtd_produtos(3).

% guarda a quantidade de corredores presentes que guardam os itens
:- dynamic qtd_corredores/1.
qtd_corredores(0).

% corredor(Num_corredor, [Lista dos produtos disponíveis (tam = qnd_produtos)])
:- dynamic corredor/2.

% Mostra os corredores atuais e seus produtos disponíveis
status_corredor :-
    qtd_corredores(N),
    N > 0,
    findall(Num-Produtos, corredor(Num, Produtos), ListaCorredores),
    keysort(ListaCorredores, ListaCorredoresOrdenada),
    write_corredores(ListaCorredoresOrdenada).

% Escreve o corredor no terminal
write_corredores([]) :- writeln(''), !.
write_corredores([Num-Produtos | T]) :-
    format('Corredor ~d: [', [Num]),
    write_produtos(Produtos),
    write_corredores(T).

% escreve os produtos disponíveis do corredor no terminal
write_produtos([Qtd | []]) :- format('~d]~n', [Qtd]), !.
write_produtos([Qtd | T]) :-
    format('~d, ', [Qtd]),
    write_produtos(T).

% registra um corredor na base de conhecimento
add_corredor(Produtos) :- qtd_produtos(N), 
                            length(Produtos, N), 
                            qtd_corredores(QtdAntiga),
                            NumCorredor is QtdAntiga + 1,
                            assertz(corredor(NumCorredor, Produtos)), 
                            retract(qtd_corredores(QtdAntiga)),
                            assertz(qtd_corredores(NumCorredor)),
                            format('corredor ~d adicionado na base de conhecimento~n', [NumCorredor]), !.
add_corredor(_) :- writeln('o corredor não pode ser adicionado a base de conhecimento'), fail.

% Corrige o número dos corredores seguidos após um corredor removido
diminuir_num_sucessores([]) :- !.
diminuir_num_sucessores([H | T]) :- corredor(H, Produtos), 
                                    retract(corredor(H, Produtos)), 
                                    Num is H - 1, assertz(corredor(Num, Produtos)), 
                                    diminuir_num_sucessores(T).

% Remove um corredor na base de conhecimento
remove_corredor(NumCorredor) :- retract(corredor(NumCorredor, _)), 
                                    findall(N, (corredor(N, _), N > NumCorredor), L), 
                                    diminuir_num_sucessores(L), 
                                    format('corredor ~d removido da base de conhecimento~n', [NumCorredor]), !.
remove_corredor(_) :- writeln('o corredor não pode ser removido da base de conhecimento'), fail.

% Checa se a lista de corredores pode satisfazer o pedido
checa_satisfacao(Pedido, Corredores) :-
    maplist(produtos_corredor, Corredores, ListaProdutos), % Cria uma lista com todos as listas de produtos de todos os corredores
    soma_produtos(ListaProdutos, SomaProdutos), 
    satisfaz_pedido(Pedido, SomaProdutos).

% Retorna os produtos de um corredor
produtos_corredor(CorridorNumber, ProductList) :-
    corredor(CorridorNumber, ProductList).

% Soma os produtos de todos os corredores
soma_produtos([], []) :- !.
soma_produtos([H | T ], Sum) :-
    soma_produtos(T, TempSoma),
    (TempSoma = [] -> Sum = H;   
    soma_listas(H, TempSoma, Sum)).

% Soma elementos de duas listas
soma_listas([], [], []) :- !.
soma_listas([X|Xs], [Y|Ys], [Z|Zs]) :-
    Z is X + Y,
    soma_listas(Xs, Ys, Zs).

subtrair_listas([], [], []) :- !.
subtrair_listas([X|Xs], [Y|Ys], [Z|Zs]) :-
    Z is X - Y,
    subtrair_listas(Xs, Ys, Zs).

% Checa se a soma dos produtos satisfaz o pedido
satisfaz_pedido([], _) :- !.
satisfaz_pedido([R|Rs], [A|As]) :-
    A >= R,
    satisfaz_pedido(Rs, As), !.

% Procura todas as combinações de corredores que conseguem satisfazer o pedido
encontrar_corredores(Pedido, Res) :-
    findall(CorridorNumber, corredor(CorridorNumber, _), TodosCorredores),
    % Gera todas as combinações não vazias
    setof(CorredoresSubLista,
          (sublista(TodosCorredores, CorredoresSubLista),
           CorredoresSubLista \= [],
           checa_satisfacao(Pedido, CorredoresSubLista)),
          Corredores),
    sort_tamanho(Corredores, Res),
    !.
encontrar_corredores(_, _) :-
    writeln('Nao ha produtos o suficiente no estoque'), fail.

% gera uma sublista
sublista([], []) :- !.
sublista([X|Xs], [X|Ys]) :-
    sublista(Xs, Ys).
sublista([_|Xs], Ys) :-
    sublista(Xs, Ys).

% Ordena as sublistas por seu tamanho de forma crescente
sort_tamanho(SubListas, Res) :-
    maplist(tam, SubListas, Pares),
    keysort(Pares, SortPares),
    pairs_values(SortPares, Res).

tam(Lista, Len-Lista) :-
    length(Lista, Len).


% Indica a posição (corredor) em que o robô se encontra
:- dynamic pos/1.
pos(0).

% Troca a posição do robô
mover(Dest) :- pos(Origem), Origem =:= Dest, !.
mover(Dest) :- pos(Origem), Origem =:= 0, retract(pos(Origem)), assertz(pos(Dest)), format('Robo foi do local de entrega para o corredor ~d~n', [Dest]), !.
mover(Dest) :- pos(Origem), Dest =:= 0, retract(pos(Origem)), assertz(pos(Dest)), format('Robo foi do corredor ~d para o local de entrega~n', [Origem]), !.
mover(Dest) :- pos(Origem), retract(pos(Origem)), assertz(pos(Dest)), format('Robo foi do corredor ~d para o corredor ~d~n', [Origem, Dest]).

% Calcula a distância do robô para outro corredor
calc_dist(Corredor, Dist) :- pos(Pos), Dist is abs(Pos - Corredor).

% Calcula um valor de eficiência de uma rota
calc_valor_sublista(SubLista, Valor) :- 
    length(SubLista, Tam),
    sum_list(SubLista, SumDist),
    Valor is 1000 / (SumDist + Tam).

% Calcula um valor de eficiência para uma lista de rotas
calc_valor([], []) :- !.
calc_valor([SubLista | T], [Valor-SubLista | R]) :-
    calc_valor(T, R),
    calc_valor_sublista(SubLista, Valor).

% Retorna o maior valor da lista de valor-rota
max_valor([X], X) :- !.
max_valor([Valor1-SubLista1 | T], Max) :-
    max_valor(T, Valor2-_),
    Valor1 >= Valor2,
    Max = Valor1-SubLista1, !.
max_valor([_-_ | T], Max) :-
    max_valor(T, Valor2-SubLista2),
    Max = Valor2-SubLista2, !.

% Salva a quantidade de produtos que o robô pode guardar de uma vez

% indica os itens que o robô tem guardado
:- dynamic mochila/1.

% Inicia os valores da mochila
mochila_inicial :- \+ mochila(_), qtd_produtos(Tam), length(L, Tam), add_valores(L), assertz(mochila(L)).
mochila_inicial :- !.

% Mostra os itens que estão sendo carregados pelo robô
status_mochila :- 
    mochila(Produtos),
    write_produtos_mochila(Produtos).

% Mostra na tela os produtos carregados pela mochila
write_produtos_mochila(Produtos) :- writeln('Produtos carregados:'), write_produtos_mochila(Produtos, 1).
write_produtos_mochila([], _) :- writeln('').
write_produtos_mochila([Qtd | T], NumProduto) :- format('Produto ~d: ~d~n', [NumProduto, Qtd]), Prox is NumProduto + 1, write_produtos_mochila(T, Prox).

% Zera todos os elementos de uma lista não instanciada
add_valores([]) :- !.
add_valores([H | T]) :- H = 0, add_valores(T).

% Atualiza a lista de produtos dentro da mochila
att_lista(_, _, [], N, []) :- qtd_produtos(N), !.
att_lista(Produto, Qtd, [H | T], I, [ValorAtt | Res]) :- 
    att_lista(Produto, Qtd, T, Ant, Res), 
    I is Ant - 1, 
    Ant =:= Produto, 
    ValorAtt is 
    Qtd + H, !.
att_lista(Produto, Qtd, [H | T], I, [H | Res]) :- 
    att_lista(Produto, Qtd, T, Ant, Res), 
    I is Ant - 1, !.

% atualiza na base de conhecimento os produtos da mochila
att_mochila(Produto, Qtd) :- 
    mochila(Produtos), 
    att_lista(Produto, Qtd, Produtos, _, AttProdutos),
    retract(mochila(Produtos)),
    assertz(mochila(AttProdutos)), !.

att_mochila(ProdutosNovos) :-
    mochila(Produtos),
    soma_listas(Produtos, ProdutosNovos, ProdutosTotal),
    retract(mochila(Produtos)),
    assertz(mochila(ProdutosTotal)).

descarregar_mochila(ProdutosRetirados) :-
    mochila(Produtos),
    subtrair_listas(Produtos, ProdutosRetirados, ProdutosTotal),
    retract(mochila(Produtos)),
    assertz(mochila(ProdutosTotal)).

% Esvazia a mochila
descarregar_mochila :- mochila(P), retract(mochila(P)), mochila_inicial.

% Decrementa os itens do corredor e os adiciona no inventário do robô
dec_itens([], [], [], [], _) :- !.
dec_itens([Precisa | T1], [Tem | T2], [PedidoAtt | T3], [CorredorAtt | T4], NumProduto) :-
    CorredorAtt is max(0, Precisa - Tem),
    Qtd is min(Precisa, Tem),
    PedidoAtt is Precisa - Qtd,
    att_mochila(NumProduto, Qtd),
    format('Robo pegou ~d item(s) do produto ~d~n', [Qtd, NumProduto]),
    Prox is NumProduto + 1,
    dec_itens(T1, T2, T3, T4, Prox).

% Atualiza as quantidades dos produtos dos corredores na base de conhecimento
pegar_itens(Pedido, NumCorredor, AttPedido) :- 
    corredor(NumCorredor, Produtos),
    dec_itens(Pedido, Produtos, AttPedido, AttProdutos, 1),
    retract(corredor(NumCorredor, Produtos)),
    assertz(corredor(NumCorredor, AttProdutos)).

% Percorre a rota de um pedido
cursar_rota(_, []) :- !.
cursar_rota(Pedido, [H | T]) :- 
    mover(H),
    format('----------- Corredor ~d ----------~n', [H]), 
    pegar_itens(Pedido, H, AttPedido), 
    format('---------------------------------~n~n'),
    cursar_rota(AttPedido, T).

% Escreve uma rota no terminal
write_rota([H | []]) :- format('~d~n', [H]), !.
write_rota([H | T]) :- format('~d -> ', [H]), write_rota(T).

% Escreve múltiplas rotas no terminal
write_candidatos([]) :- writeln(''), !.
write_candidatos([H | T]) :- write_rota(H), write_candidatos(T).

% Realiza um pedido e o robô o cumpre
fazer_pedido(Pedido) :- 
    qtd_produtos(Qtd), 
    length(Pedido, Qtd), 
    mochila_inicial,
    writeln('Pedido Recebido! Procurando rota...'),
    encontrar_corredores(Pedido, Candidatos),
    writeln('Possiveis rotas encontradas!'),
    write_candidatos(Candidatos),
    writeln('Encontrando melhor rota...'),
    calc_valor(Candidatos, ValorCandidatos),
    max_valor(ValorCandidatos, _-MelhorRota),
    writeln('Melhor rota Encontrada!'),
    write_rota(MelhorRota),
    cursar_rota(Pedido, MelhorRota),
    writeln('Rota concluida!'),
    status_mochila,
    !.

entregar_pedido :- 
    descarregar_mochila,
    mover(0).

recarregar_corredor(NumCorredor, _) :- 
    mochila_inicial,
    format('Recarregamento de produtos do corredor ~d recebido!~n', [NumCorredor]),
    pos(Pos), 
    Pos > 0, 
    mover(0).
recarregar_corredor(NumCorredor, ProdutosNovos) :- 
    att_mochila(ProdutosNovos),
    writeln('Produtos coletados!'),
    corredor(NumCorredor, Produtos),
    mover(NumCorredor),
    descarregar_mochila(ProdutosNovos),
    soma_listas(Produtos, ProdutosNovos, ProdutosAtt),
    retract(corredor(NumCorredor, Produtos)),
    assertz(corredor(NumCorredor, ProdutosAtt)),
    format('Produtos adicionados no corredor ~d~n', [NumCorredor]),
    writeln('Recarregamento concluído!').
