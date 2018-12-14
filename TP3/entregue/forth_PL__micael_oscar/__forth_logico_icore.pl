:- encoding(utf8).
:- ['__forth_logico_core', '__forth_logico_utils'].

/*
 * Considerações sobre este arquivo:
 * - falha: algumas regras usam o `interprete` (regra fora do contexto)
 * - disponibilizar uma API do core para o programa principal
 * - residem regras "wrappers" para garantir uma aridade 2 (+Stacks, -StacksAtualizadas)
 * - um predicado não pode ter mais de 1 definição
 * - todas as regras podem usar [diretamente] regras do core
 * - todas as regras devem dar True sempre que consultadas, para evitar erros/backtrack no `interprete`
 */


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


%! top_to_bool(+PilhaAtual, -PilhaNova, -TopoBooleano)
%  Realiza um casting do topo de PilhaAtual (força 0 em erros).
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



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%% API %%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%! primitiva(?CmdRepresentation, ?RuleName)
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

%! primitiva_com_delimitadores(?CmdInicio, CmdFim, ?Acumulator, RuleName)
primitiva_com_delimitadores(':',     ';',    acumulate_new_primitive, new_primitive).
primitiva_com_delimitadores('."',    '"',         acumulate_to_print, to_print).
primitiva_com_delimitadores('begin', 'until',  acumulate_begin_until, begin_until).
primitiva_com_delimitadores('if',     'then', acumulate_if_else_then, if_else_then).
