% Configurações
qtd_produtos(5).
max_produtos(12).

:- dynamic qtd_corredores/1.
qtd_corredores(0).

% corredor(Num_corredor, [Lista dos produtos disponíveis (tam = qnd_produtos)])
:- dynamic corredor/2.

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

remove_corredor(NumCorredor) :- retract(corredor(NumCorredor, _)), 
                                    findall(N, (corredor(N, _), N > NumCorredor), L), 
                                    diminuir_num_sucessores(L), 
                                    format('corredor ~d removido da base de conhecimento~n', [NumCorredor]), !.
remove_corredor(_) :- writeln('o corredor não pode ser removido da base de conhecimento'), fail.

:- dynamic pedido/2.

% calc_num_pedido(Res) :- findall(N, pedido(N, _), L), length(L, Num), Res is Num + 1, !.

% add_pedido(Produtos) :- qtd_produtos(N), 
%                             length(Produtos, N), 
%                             calc_num_pedido(Num_pedido), 
%                             assertz(pedido(Num_pedido, Produtos)), 
%                             format('pedido ~d adicionado na base de conhecimento~n', [Num_pedido]), !.
% add_pedido(_) :- writeln('o pedido não pode ser adicionado a base de conhecimento'), fail.

% % Corrige o número dos corredores seguidos após um corredor removido
% diminuir_num_sucessores_p([]) :- !.
% diminuir_num_sucessores_p([H | T]) :- pedido(H, Produtos), 
%                                     retract(pedido(H, Produtos)), 
%                                     Num is H - 1, assertz(pedido(Num, Produtos)), 
%                                     diminuir_num_sucessores_p(T).

% remove_pedido(Num_pedido) :- retract(pedido(Num_pedido, _)), 
%                                     findall(N, (pedido(N, _), N > Num_pedido), L), 
%                                     diminuir_num_sucessores(L), 
%                                     format('pedido ~d removido da base de conhecimento~n', [Num_pedido]), !.
% remove_pedido(_) :- writeln('o pedido não pode ser removido da base de conhecimento'), fail.

% Predicate to check if a list of Corredores can satisfaz_pedido the Pedido
checa_satisfacao(Pedido, Corredores) :-
    % Get the product lists for all Corredores
    maplist(produtos_corredor, Corredores, ListaProdutos),
    % Sum the products across all Corredores
    soma_produtos(ListaProdutos, SomaProdutos),
    % Check if the sum meets the Pedido
    satisfaz_pedido(Pedido, SomaProdutos).

% Helper predicate to check if a corridor exists
existe_corredor(CorridorNumber) :-
    corredor(CorridorNumber, _).

% Helper predicate to get the product list of a corridor
produtos_corredor(CorridorNumber, ProductList) :-
    corredor(CorridorNumber, ProductList).

% Predicate to sum the products across multiple Corredores
soma_produtos([], []) :- !.
soma_produtos([H | T ], Sum) :-
    soma_produtos(T, TempSoma),
    (   TempSoma = [] -> Sum = H
    ;   soma_listas(H, TempSoma, Sum)
    ).

% Helper predicate to sum two lists element-wise
soma_listas([], [], []) :- !.
soma_listas([X|Xs], [Y|Ys], [Z|Zs]) :-
    Z is X + Y,
    soma_listas(Xs, Ys, Zs).

% Predicate to check if the sum of products meets the Pedido
satisfaz_pedido([], _) :- !.
satisfaz_pedido([R|Rs], [A|As]) :-
    A >= R,
    satisfaz_pedido(Rs, As), !.

% Predicate to find all combinations of Corredores that satisfaz_pedido the Pedido
encontrar_corredores(Pedido, Res) :-
    % Find all corridor numbers
    findall(CorridorNumber, corredor(CorridorNumber, _), TodosCorredores),
    % Generate all possible non-empty SubListas of Corredores
    setof(CorredoresSubLista,
          (sublista(TodosCorredores, CorredoresSubLista),
           CorredoresSubLista \= [],
           checa_satisfacao(Pedido, CorredoresSubLista)),
          Corredores),
    sort_tamanho(Corredores, Res),
    !.
encontrar_corredores(_, _) :-
    writeln('Nao ha produtos o suficiente no estoque'), fail.

% Helper predicate to generate all SubListas of a list
sublista([], []) :- !.
sublista([X|Xs], [X|Ys]) :-
    sublista(Xs, Ys).
sublista([_|Xs], Ys) :-
    sublista(Xs, Ys).

sort_tamanho(SubListas, Res) :-
    maplist(tam, SubListas, Pares),
    keysort(Pares, SortPares),
    pairs_values(SortPares, Res).

tam(Lista, Len-Lista) :-
    length(Lista, Len).


:- dynamic pos/1.

mover(Dest) :- pos(Origem), Origem =:= Dest, !.
mover(Dest) :- pos(Origem), retract(pos(Origem)), assertz(pos(Dest)), format('Robo foi do corredor ~d para o corredor ~d~n', [Origem, Dest]).

pos_inicial :- \+ pos(_), qtd_corredores(N), Pos is div(N, 2), assertz(pos(Pos)), !.
pos_inicial :- !.

calc_dist(Corredor, Dist) :- pos(Pos), Dist is abs(Pos - Corredor).

sum_dist([], 0) :- !.
sum_dist([Corredor | T], Res) :- sum_dist(T, Sum), calc_dist(Corredor, Dist), Res is Sum + Dist.

calc_valor_sublista(SubLista, Valor) :- 
    length(SubLista, Tam),
    sum_list(SubLista, SumDist),
    Valor is 1000 / (SumDist + Tam).

calc_valor([], []) :- !.
calc_valor([SubLista | T], [Valor-SubLista | R]) :-
    calc_valor(T, R),
    calc_valor_sublista(SubLista, Valor).

max_valor([X], X) :- !.
max_valor([Valor1-SubLista1 | T], Max) :-
    max_valor(T, Valor2-_),
    Valor1 >= Valor2,
    Max = Valor1-SubLista1, !.
max_valor([_-_ | T], Max) :-
    max_valor(T, Valor2-SubLista2),
    Max = Valor2-SubLista2, !.

:- dynamic mochila/1.

add_valores([]) :- !.
add_valores([H | T]) :- H = 0, add_valores(T).

mochila_inicial :- \+ mochila(_), qtd_produtos(Tam), length(L, Tam), add_valores(L), assertz(mochila(L)).
mochila_inicial :- !.

att_lista(_, _, [], N, []) :- qtd_produtos(N), !.
att_lista(Produto, Qtd, [H | T], I, [ValorAtt | Res]) :- att_lista(Produto, Qtd, T, Ant, Res), I is Ant - 1, Ant =:= Produto, ValorAtt is Qtd + H, !.
att_lista(Produto, Qtd, [H | T], I, [H | Res]) :- att_lista(Produto, Qtd, T, Ant, Res), I is Ant - 1, !.

att_mochila(Produto, Qtd) :- 
    mochila(Produtos), 
    sum_list(Produtos, QtdProdutos), 
    max_produtos(MaxProdutos), 
    (QtdProdutos + Qtd) =< MaxProdutos, 
    att_lista(Produto, Qtd, Produtos, _, AttProdutos),
    retract(mochila(Produtos)),
    assertz(mochila(AttProdutos)), !.

dec_itens([], [], [], [], _) :- !.
dec_itens([Precisa | T1], [Tem | T2], [PedidoAtt | T3], [CorredorAtt | T4], NumProduto) :-
    CorredorAtt is max(0, Precisa - Tem),
    Qtd is min(Precisa, Tem),
    PedidoAtt is Precisa - Qtd,
    att_mochila(NumProduto, Qtd),
    format('Robo pegou ~d item(s) do produto ~d~n', [Qtd, NumProduto]),
    Prox is NumProduto + 1,
    dec_itens(T1, T2, T3, T4, Prox).

pegar_itens(Pedido, NumCorredor, AttPedido) :- 
    corredor(NumCorredor, Produtos),
    dec_itens(Pedido, Produtos, AttPedido, AttProdutos, 1),
    retract(corredor(NumCorredor, Produtos)),
    assertz(corredor(NumCorredor, AttProdutos)).

cursar_rota(_, []) :- !.
cursar_rota(Pedido, [H | T]) :- 
    mover(H),
    format('----------- Corredor ~d ----------~n', [H]), 
    pegar_itens(Pedido, H, AttPedido), 
    format('---------------------------------~n~n'),
    cursar_rota(AttPedido, T).

write_rota([H | []]) :- format('~d~n', [H]), !.
write_rota([H | T]) :- format('~d -> ', [H]), write_rota(T).

write_candidatos([]) :- writeln(''), !.
write_candidatos([H | T]) :- write_rota(H), write_candidatos(T).

fazer_pedido(Pedido) :- 
    qtd_produtos(Qtd), 
    length(Pedido, Qtd), 
    pos_inicial, 
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
    !.