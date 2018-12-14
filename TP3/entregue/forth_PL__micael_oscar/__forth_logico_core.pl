:- encoding(utf8).

/*
 * Considerações sobre este arquivo:
 * - a pilha é um `list`
 * - o topo da pilha é o primeiro elemento da `list`
 * - todas os predicados devem atuar sobre pilhas
 * - tornar mínimo & legível; manter só o essencial
 * - nenhum predicado deve usar predicados que estão sendo definidos aqui
 */


%! top(+Pilha, -Topo)
%  True se Topo eh o primeiro elemento da lista Pilha.
top([Topo|_], Topo).

%! push(+PilhaAtual, +Val, -PilhaNova)
%  True se Val eh o primeiro elemento de PilhaNova, com PilhaAtual
%  sendo o resto.
push([], Val, [Val]).
push([TopoAntigo|RestoPilha], Val, [Val, TopoAntigo | RestoPilha]).

%! pop(+PilhaAtual, -PilhaNova)
%  True se PilhaNova eh a PilhaAtual sem o primeiro elemento.
pop([_|PilhaNova], PilhaNova) :- !.
pop(Pilha, Pilha).

%! bin_op(+Operador, +PilhaAtual, -PilhaNova)
%  Realizar uma operacao binaria com os topos de PilhaAtual.
bin_op(Operador, [Topo1,Topo2 | Resto], [Resultado|Resto]) :-
  Goal =.. [Operador, Topo1, Topo2],
  Resultado is Goal.

%! una_op(+Operador, +PilhaAtual, -PilhaNova)
%  Realizar uma operacao unaria com o topo de PilhaAtual.
una_op(Operador, [Topo|Resto], [Resultado|Resto]) :-
  Goal =.. [Operador, Topo],
  Resultado is Goal.


%! swap(+PilhaAtual, -PilhaNova)
%  True se PilhaNova eh a PilhaAtual com os dois elementos do topo
%  alterados.
c_swap([Topo1,Topo2 | Resto], [Topo2,Topo1 | Resto]) :- !.
c_swap(Pilha, Pilha).

%! dot(+PilhaAtual, -PilhaNova)
%  True se PilhaNova eh PilhaAtual sem o primeiro elemento & print nele.
c_dot([], []) :- write('?').
c_dot([H|T], T) :- write(H).

%! greater_than(+PilhaAtual, -PilhaNova)
%  True se PilhaNova tem como topo 1 ou 0 se acordo com a relacao MAIOR
%  QUE entre os dois topos de PilhaAtual, onde: o primeiro operando eh o
%  segundo topo.
c_greater_than([Topo1,Topo2 | Resto], [1|Resto]) :- Topo2 > Topo1, !.
c_greater_than([_,_ | Resto], [0|Resto]) :- !.
c_greater_than(Pilha, Pilha).

%! less_than(+PilhaAtual, -PilhaNova)
%  True se PilhaNova tem como topo 1 ou 0 se acordo com a relacao MENOR
%  QUE entre os dois topos de PilhaAtual, onde: o primeiro operando eh o
%  segundo topo.
c_less_than([Topo1,Topo2 | Resto], [BinResult|Resto]) :- (Topo2 < Topo1 -> BinResult is 1 ; BinResult is 0), !.
c_less_than(Pilha, Pilha).

%! equal(+PilhaAtual, -PilhaNova)
%  True se PilhaNova tem como topo 1 se os dois topos de PilhaAtual
%  forem iguais; 0, caso contrario.
c_equal([Topo1,Topo2 | Resto], [B|Resto]) :- (Topo1 =:= Topo2 -> B is 1 ; B is 0), !.
c_equal(Pilha, Pilha).

%! or(+PilhaAtual, -PilhaNova)
%  True se PilhaNova tem como topo 1 se um dos dois topos de PilhaAtual
%  for diferente de 0 (considerado como valor falso do Forth).
c_or([Topo1,Topo2 | Resto], [B|Resto]) :- ( (Topo1=\=0 ; Topo2=\=0) -> B is 1 ; B is 0 ), !.
c_or(Pilha, Pilha).

%! and(+PilhaAtual, -PilhaNova)
%  True se PilhaNova tem como topo 1 se os dois topos de PilhaAtual
%  forem diferentes de 0 (considerado como valor falso do Forth).
c_and([Topo1,Topo2 | Resto], [B|Resto]) :- ( (Topo1=\=0 , Topo2=\=0) -> B is 1 ; B is 0 ), !.
c_and(Pilha, Pilha).

%! not(+PilhaAtual, -PilhaNova)
%  True se PilhaNova tem como topo 1 se o topo de PilhaAtual for igual a
%  0 (considerado como valor falso do Forth); 1, caso contrario.
c_not([], []).
c_not([Topo|Resto], [B|Resto]) :- Topo =:= 0 -> B is 1 ; B is 0.

%! empty(+PilhaIntermediaria, +PilhaAtual, -PilhaNova)
%  True se PilhaNova tem como topo 1 e resto PilhaAtual, se
%  PilhaIntermediaria estiver vazia; 0, caso contrario.
c_empty(PilhaIntermediaria, PilhaAtual, [BinResult | PilhaAtual]) :-
  length(PilhaIntermediaria, Tam),
  Tam > 0 -> BinResult is 0 ; BinResult is 1.

%! dup(+PilhaAtual, -PilhaNova)
%  True se PilhaNova tem como topo e segundo topo, o topo de PilhaAtual.
c_dup([], []).
c_dup([Topo|Resto], [Topo,Topo |Resto]).

%! move_top(+Pilha1, +Pilha2, -Pilha1Nova, -Pilha2Nova)
%  True se Pilha1Nova eh Pilha1 sem o topo da mesma. E Pilha2Nova eh
%  Pilha2 com esse topo (de Pilha1).
c_move_top([], P2, [], P2).
c_move_top([T1|RestoPilha1], [], RestoPilha1, [T1]).
c_move_top([T1|RestoPilha1], [T2|R2], RestoPilha1, [T1,T2 |R2]).

%! copy_top(+Pilha1, +Pilha2, -Pilha2Nova)
%  True se Pilha2Nova eh Pilha2 com o topo de Pilha1.
c_copy_top([], P2, P2).
c_copy_top([T|_], P2, [T|P2]).

%! roll(+PilhaAtual, -PilhaNova)
%  True se PilhaNova eh a PilhaAtual com n-esimo valor dela movido pro
%  topo da pilha, onde o `n` eh o topo de PilhaAtual.
c_roll([T|R], [NEsimo|Resto]) :- nth0(T, R, NEsimo, Resto), !.
c_roll(Pilha, Pilha).
