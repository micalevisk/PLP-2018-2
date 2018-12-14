:- encoding(utf8).
:- ['__forth_logico_icore'].
:- dynamic user_defs/2.


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
