%%%%%% commit ba784f4
:- encoding(utf8).
:- dynamic user_defs/2.


/***********************************************************************************************************************************************/

top([Topo|_], Topo).

push([], Val, [Val]).
push([TopoAntigo|RestoPilha], Val, [Val, TopoAntigo | RestoPilha]).

pop([_|PilhaNova], PilhaNova) :- !.
pop(Pilha, Pilha).

bin_op(Operador, [Topo1,Topo2 | Resto], [Resultado|Resto]) :-
  Goal =.. [Operador, Topo1, Topo2],
  Resultado is Goal.

una_op(Operador, [Topo|Resto], [Resultado|Resto]) :-
  Goal =.. [Operador, Topo],
  Resultado is Goal.


c_swap([Topo1,Topo2 | Resto], [Topo2,Topo1 | Resto]) :- !.
c_swap(Pilha, Pilha).

c_dot([], []) :- write('?').
c_dot([H|T], T) :- write(H).

c_greater_than([Topo1,Topo2 | Resto], [1|Resto]) :- Topo2 > Topo1, !.
c_greater_than([_,_ | Resto], [0|Resto]) :- !.
c_greater_than(Pilha, Pilha).

c_less_than([Topo1,Topo2 | Resto], [BinResult|Resto]) :- (Topo2 < Topo1 -> BinResult is 1 ; BinResult is 0), !.
c_less_than(Pilha, Pilha).

c_equal([Topo1,Topo2 | Resto], [B|Resto]) :- (Topo1 =:= Topo2 -> B is 1 ; B is 0), !.
c_equal(Pilha, Pilha).

c_or([Topo1,Topo2 | Resto], [B|Resto]) :- ( (Topo1=\=0 ; Topo2=\=0) -> B is 1 ; B is 0 ), !.
c_or(Pilha, Pilha).

c_and([Topo1,Topo2 | Resto], [B|Resto]) :- ( (Topo1=\=0 , Topo2=\=0) -> B is 1 ; B is 0 ), !.
c_and(Pilha, Pilha).

c_not([], []).
c_not([Topo|Resto], [B|Resto]) :- Topo =:= 0 -> B is 1 ; B is 0.

c_empty(PilhaIntermediaria, PilhaAtual, [BinResult | PilhaAtual]) :-
  length(PilhaIntermediaria, Tam),
  Tam > 0 -> BinResult is 0 ; BinResult is 1.

c_dup([], []).
c_dup([Topo|Resto], [Topo,Topo |Resto]).

c_move_top([], P2, [], P2).
c_move_top([T1|RestoPilha1], [], RestoPilha1, [T1]).
c_move_top([T1|RestoPilha1], [T2|R2], RestoPilha1, [T1,T2 |R2]).

c_copy_top([], P2, P2).
c_copy_top([T|_], P2, [T|P2]).

c_roll([T|R], [NEsimo|Resto]) :- nth0(T, R, NEsimo, Resto), !.
c_roll(Pilha, Pilha).


/***********************************************************************************************************************************************/

assertz_unique(OldFact, NewFact) :-
  ( call(OldFact) -> retract(OldFact) ; true ),
  assertz(NewFact).


slice([H|_], 0, 1, [H]).

slice([H|T], 0, To, [H|Ys]) :-
  N is To - 1,
  slice(T, 0, N, Ys), !.

slice([_|T], From, To, L) :-
  N is From - 1,
  M is To - 1,
  slice(T, N, M, L), !.

slice(List, From, L) :-
  length(List, Len),
  slice(List, From, Len, L).


split_by(_, [], [], []) :- !.
split_by(V, [V|R], [], R) :- !.
split_by(V, L, AcumLeft, AcumRight) :-
  nth0(Index, L, V),
  slice(L, 0, Index, AcumLeft),
  slice(L, Index, [_|AcumRight]), !.



/***********************************************************************************************************************************************/

disp([WS, RS], [WS, RS]) :-
  reverse(WS, WSInversa),
  write('working stack = '), writeln(WSInversa),
  reverse(RS, RSInversa),
  write('result  stack = '), writeln(RSInversa).

sum([WS, RS], [WSAtualizada, RS]) :- bin_op(+, WS, X) -> WSAtualizada = X ; WSAtualizada = WS.
sub([WS, RS], [WSAtualizada, RS]) :- bin_op(-, WS, X) -> WSAtualizada = X ; WSAtualizada = WS.
mul([WS, RS], [WSAtualizada, RS]) :- bin_op(*, WS, X) -> WSAtualizada = X ; WSAtualizada = WS.
div([WS, RS], [WSAtualizada, RS]) :- bin_op(/, WS, X) -> WSAtualizada = X ; WSAtualizada = WS.
mod([WS, RS], [WSAtualizada, RS]) :- bin_op(mod, WS, X) -> WSAtualizada = X ; WSAtualizada = WS.
sqrt([WS, RS], [WSAtualizada, RS]) :- una_op(sqrt, WS, X) -> WSAtualizada = X ; WSAtualizada = WS.

cr(Stacks, Stacks) :- nl.

swap([WS,RS], [WSAtualizada, RS]) :- c_swap(WS, WSAtualizada).

dot([WS, RS], [WSAtualizada, RS]) :- c_dot(WS, WSAtualizada).

greater_than([WS, RS], [WSAtualizada, RS]) :- c_greater_than(WS, WSAtualizada).
less_than([WS, RS], [WSAtualizada, RS]) :- c_less_than(WS, WSAtualizada).
equal([WS, RS], [WSAtualizada, RS]) :- c_equal(WS, WSAtualizada).

or([WS, RS], [WSAtualizada, RS]) :- c_or(WS, WSAtualizada).
and([WS, RS], [WSAtualizada, RS]) :- c_and(WS, WSAtualizada).
not([WS, RS], [WSAtualizada, RS]) :- c_not(WS, WSAtualizada).

empty([WS, RS], [WSAtualizada, RS]) :- c_empty(WS, WS, WSAtualizada).
rempty([WS, RS], [WSAtualizada, RS]) :- c_empty(RS, WS, WSAtualizada).

drop([WS, RS], [WSAtualizada, RS]) :- pop(WS, WSAtualizada).

dup([WS, RS], [WSAtualizada, RS]) :- c_dup(WS, WSAtualizada).

roll([WS, RS], [WSAtualizada, RS]) :- c_roll(WS, WSAtualizada).

pick([WS, RS], [WSAtualizada, RS]) :-
  top(WS, N),
  length(WS, TamWS),
  N < TamWS - 1 ->
  ( c_roll(WS, WSTemp), c_dup(WSTemp, WSAtualizada) ) ;
  WSAtualizada = WS. %% se N for invalido ou `top` falhar

move_ws_to_rs([WS, RS], [WSAtualizada, RSAtualizada]) :- c_move_top(WS, RS, WSAtualizada, RSAtualizada).
move_rs_to_ws([WS, RS], [WSAtualizada, RSAtualizada]) :- c_move_top(RS, WS, RSAtualizada, WSAtualizada).
copy_ws_to_rs([WS, RS], [WS, RSAtualizada]) :- c_copy_top(WS, RS, RSAtualizada).

rot_3(StacksAntigas, StacksAtualizadas) :-
  swap(StacksAntigas, S1),
  move_ws_to_rs(S1, S2),
  swap(S2, S3),
  move_rs_to_ws(S3, StacksAtualizadas).


top_to_bool(Pilha, PilhaAtualizada, TopoBooleano) :-
  c_dup(Pilha, P1),
  c_not(P1, P2),
  c_not(P2, P3),
  top(P3, TopoBooleano) ->
  pop(Pilha, PilhaAtualizada) ;
  TopoBooleano is 0.


loop_until_cond(Seq, [WS, RS], NStacks) :-
  top_to_bool(WS, WSAfterCond, CondBooleano),
  (
    CondBooleano =:= 0 ->
      (
        interprete(Seq, continuar, [WSAfterCond, RS], StacksAfterEval),
        loop_until_cond(Seq, StacksAfterEval, NStacks)
      )
    ;
      (
        NStacks = [WSAfterCond, RS]
      )
  ).

acumulate_new_primitive(MarcadorFim, [NovaPalavra|CmdBody], OutrosCmds, [NovaPalavra, Seq]) :-
  NovaPalavra \= ':',
  length(CmdBody, TamanhoBody),
  TamanhoBody > 0,
  split_by(MarcadorFim, CmdBody, Seq, OutrosCmds).


acumulate_to_print(MarcadorFim, CmdBody, OutrosCmds, [Seq]) :-
  split_by(MarcadorFim, CmdBody, Seq, OutrosCmds).


acumulate_begin_until(MarcadorFim, CmdBody, OutrosCmds, [Seq]) :-
  split_by(MarcadorFim, CmdBody, Seq, OutrosCmds).


acumulate_if_else_then(MarcadorFim, CmdBody, OutrosCmds, [Seq1, Seq2]) :-
  split_by(MarcadorFim, CmdBody, ProcBody, OutrosCmds),
  (
    member(else, ProcBody) ->
      split_by(else, ProcBody, Seq1, Seq2)
      ;
      (Seq1 = ProcBody , Seq2 = [])
  ).


to_print([TokensAcumulados], Stacks, Stacks) :-
  atomics_to_string(TokensAcumulados, ' ', Concatenados),
  write(Concatenados).


if_else_then([Seq1, Seq2], [WS, RS], NStacks) :-
  length(Seq1, TamSeq1), TamSeq1 > 0,
  top_to_bool(WS, WSAfterCond, CondBooleano),
  (
    CondBooleano =:= 0 ->
      interprete(Seq2, continuar, [WSAfterCond, RS], NStacks)
    ;
      interprete(Seq1, continuar, [WSAfterCond, RS], NStacks)
  ).


new_primitive([Palavra, Seq], Stacks, Stacks) :-
  length(Seq, TamSeq), TamSeq > 0,
  assertz_unique(
    user_defs(Palavra, _),
    user_defs(Palavra, Seq)
  ).


begin_until([Seq], Stacks, StacksAfterLoop) :-
  length(Seq, TamSeq), TamSeq > 0,
  interprete(Seq, continuar, Stacks, StacksAfterFirstSeq),
  loop_until_cond(Seq, StacksAfterFirstSeq, StacksAfterLoop).

primitiva('swap',   swap).
primitiva('disp',   disp).
primitiva('.',      dot).
primitiva('cr',     cr).
primitiva('+',      sum).
primitiva('-',      sub).
primitiva('*',      mul).
primitiva('/',      div).
primitiva('%',      mod).
primitiva('sqrt',   sqrt).
primitiva('>',      greater_than).
primitiva('<',      less_than).
primitiva('=',      equal).
primitiva('or',     or).
primitiva('and',    and).
primitiva('not',    not).
primitiva('empty',  empty).
primitiva('rempty', rempty).
primitiva('drop',   drop).
primitiva('dup',    dup).
primitiva('roll',   roll).
primitiva('pick',   pick).
primitiva('>r',     move_ws_to_rs).
primitiva('r>',     move_rs_to_ws).
primitiva('r@',     copy_ws_to_rs).
primitiva('rot',    rot_3).

primitiva_com_delimitadores(':',     ';',    acumulate_new_primitive, new_primitive).
primitiva_com_delimitadores('."',    '"',         acumulate_to_print, to_print).
primitiva_com_delimitadores('begin', 'until',  acumulate_begin_until, begin_until).
primitiva_com_delimitadores('if',     'then', acumulate_if_else_then, if_else_then).


/***********************************************************************************************************************************************/

interprete([], continuar, Stacks, Stacks).

%%% se for numero
interprete([CmdName|OutrosCmds], ProxCmd, [WS, RS], NStacks) :-
  atom_number(CmdName, Numero),
  push(WS, Numero, WSAtualizada),
  interprete(OutrosCmds, ProxCmd, [WSAtualizada, RS], NStacks), !.

%%% se for primitiva (simples)
interprete([CmdName|OutrosCmds], ProxCmd, Stacks, NStacks) :-
  primitiva(CmdName, Cmd),
  call(Cmd, Stacks, StacksAfterCmd), %% assume que todos as `primitivas` tem aridade 2
  interprete(OutrosCmds, ProxCmd, StacksAfterCmd, NStacks), !.

%%% se for primitiva com marcadores de inicio e fim
interprete([CmdName|CmdBody], ProxCmd, Stacks, NStacks) :-
  primitiva_com_delimitadores(CmdName, MarcadorFim, Acumulador, Cmd),
  call(Acumulador, MarcadorFim, CmdBody, OutrosCmds, Acumulados),
  call(Cmd, Acumulados, Stacks, StacksAfterCmd),
  interprete(OutrosCmds, ProxCmd, StacksAfterCmd, NStacks), !.

%%% se for primitiva definida pelo usuario
interprete([CmdName|OutrosCmds], ProxCmd, Stacks, NStacks) :-
  user_defs(CmdName, Seq),
  interprete(Seq, continuar, Stacks, StacksAfterSeq),
  interprete(OutrosCmds, ProxCmd, StacksAfterSeq, NStacks), !.

%%% to quit
interprete([quit], parar, _, _) :- !.
interprete([q], parar, _, _) :- !.

interprete([CmdName|OutrosCmds], ProxCmd, Stacks, NStacks) :-
  format('  Unknown word: [~w] ~n', CmdName),
  interprete(OutrosCmds, ProxCmd, Stacks, NStacks).


%%% interface via teclado
prompt(L) :-
  write('$ '),
  read_line_to_codes(user_input, Cs),
  atom_codes(A, Cs),
  atomic_list_concat(LTemp, ' ', A),
  delete(LTemp, '', L). %% para remover os brancos


interaja(parar, _) :-
  retractall( user_defs(_, _) ),
  writeln('Tchau!'), !.

interaja(_, Stacks) :-
  prompt(L),
  interprete(L, ProxCmd, Stacks, NStacks), nl,
  interaja(ProxCmd, NStacks).


go :-
  retractall( user_defs(_, _) ),
  writeln('ulikeForth by Micael Levi. Type "quit" or "q" to exit'),
  interaja(continuar, [[], []]).


:- initialization(go).
