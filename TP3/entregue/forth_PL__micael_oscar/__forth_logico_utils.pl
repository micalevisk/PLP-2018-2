%! assertz_unique(+OldFact, +NewFact)
%  Remove o OldFact e insere o fato NewFact, se o primeiro existir.
%  Senao, so insere NewFact.
assertz_unique(OldFact, NewFact) :-
  ( call(OldFact) -> retract(OldFact) ; true ),
  assertz(NewFact).

%! slice(+List, +From, +To, -ResultList)
%  True se ResultList tem os elementos de List a partir do indice From
%  ate o indice To.
slice([H|_], 0, 1, [H]).

slice([H|T], 0, To, [H|Ys]) :-
  N is To - 1,
  slice(T, 0, N, Ys), !.

slice([_|T], From, To, L) :-
  N is From - 1,
  M is To - 1,
  slice(T, N, M, L), !.

%! slice(+List, +From, -ResultList)
%  True se ResultList tem os elementos de List a partir do indice From.
slice(List, From, ResultList) :-
  length(List, Len),
  slice(List, From, Len, ResultList).

%! split_by(+Value, +List, -AcumLeft, +AcumRight)
%  True se AcumLeft tem os valores de List que foram encontrados ate a
%  primeira ocorrencia de Value. E AcumRight tem os demais elementos.
%  Ambos sem o elemento Value. False se Value nao for encontrado na
%  lista; exceto se List estiver vazia.
split_by(_, [], [], []) :- !.
split_by(V, [V|R], [], R) :- !.
split_by(V, L, AcumLeft, AcumRight) :-
  nth0(Index, L, V),
  slice(L, 0, Index, AcumLeft),
  slice(L, Index, [_|AcumRight]), !.
