## usando Python 3

from math import sqrt
from functools import reduce


def empty(s):
  return len(s) == 0

def push(stack, e):
  return stack + [e]

def pop(stack):
  return stack[:-1]

def top(stack):
  return stack[-1] if not empty(stack) else None

def split_val(l, v):
  if not l or l[0] == v:
    return ([], l)

  lanteriores, lresto = split_val(l[1:], v)
  return (l[:1] + lanteriores, lresto)

def bin_op(s, fb):
  if len(s) < 2:
    return s[:]
  new_s = pop(s)
  return push( pop(new_s), fb(top(s), top(new_s)) ) ## tomando o topo como o primeiro operando

def una_op(s, fu):
  return [] if empty(s) else push( pop(s), fu(top(s)) )

def swap(s):
  if empty(s):
    return []
  if len(s) < 2: ## tratando uma exceção
    return s[:]
  new_s = pop(s)
  return push( push( pop(new_s), top(s) ), top(new_s) )

def roll(s):
  if empty(s):
    return []
  if top(s) >= len(s) - 1: ## guard clasule para evitar exceções
    return s[:]

  def split_until_0(i, sin, sout):
    ## Desempilha `sin` em `sout` enquanto `i` for maior que 1
    return (sin, sout) if i < 1 \
           else split_until_0( i - 1, pop(sin), push(sout, top(sin)) )

  def repush(sin, sout):
    ## Desempilha os elementos de `sin`, empilhando-os em `sout`
    return sout if empty(sin) \
           else repush( pop(sin), push(sout, top(sin)) )

  sin, sout = split_until_0(top(s), pop(s), [])
  return push( repush(sout, pop(sin)), top(sin) )

## ========================================================================== ##
## ========================== fornecido pelo prof =========================== ##
def top_value(s):
  'shows working stack top (`?` if working stack is empty)'
  return '?' if empty(s) else '%f' % top(s) if top(s)%1 != 0 else '%d' % top(s)

def dot(ws):
  'pops working stack top and show it'
  print(top_value(ws), end='')
  return pop(ws)
## ========================================================================== ##


WS = lambda stacks: stacks[0] ## retorna a Working Stack
RS = lambda stacks: stacks[1] ## retorna a Stack de resultados



def _disp(stacks):
  'exibe na tela as duas pilhas, sem consumi-las'
  print('working stack =', WS(stacks))
  print('result  stack =', RS(stacks))
  return [ WS(stacks), RS(stacks) ]

def _sub(stacks):
  'realiza a subtração dos dois valores no topo da pilha de trabalho'
  return [ bin_op(WS(stacks), lambda x,y: x-y),
           RS(stacks) ]

def _mul(stacks):
  'realiza a multiplicação dos dois valores no topo da pilha de trabalho'
  return [ bin_op(WS(stacks), lambda x,y: x*y),
           RS(stacks) ]

def _div(stacks):
  'realiza a divisão dos dois valores no topo da pilha de trabalho'
  return [ bin_op(WS(stacks), lambda x,y: x/y),
           RS(stacks) ]

def _mod(stacks):
  'realiza a operação de resto entre os dois valores no topo da pilha de trabalho'
  return [ bin_op(WS(stacks), lambda x,y: x%y),
           RS(stacks) ]

def _greater_than(stacks):
  'substitui os dois valores do topo da pilha de trabalho pela comparação MAIOR QUE entre eles (1 ou 0)'
  result_stack = bin_op(WS(stacks), lambda x,y: y>x) ## o topo é o segundo operando
  return [ push( pop(result_stack), +top(result_stack) ),
           RS(stacks) ]

def _less_than(stacks): ## não trata divisões por 0
  'substitui os dois valores do topo da pilha de trabalho pela comparação MENOR QUE entre eles (1 ou 0)'
  result_stack = bin_op(WS(stacks), lambda x,y: y<x) ## o topo é o segundo operando
  return [ push( pop(result_stack), +top(result_stack) ),
           RS(stacks) ]

def _equal(stacks):
  'substitui os dois valores do topo da pilha de trabalho pela comparação IGUAL entre eles (1 ou 0)'
  result_stack = bin_op(WS(stacks), lambda x,y: x==y)
  return [ push( pop(result_stack), +top(result_stack) ),
           RS(stacks) ]

def _or(stacks):
  'substitui os dois valores do topo da pilha de trabalho pela comparação OR (do python) entre eles (1 ou 0)'
  result_stack = bin_op(WS(stacks), lambda x,y: x or y)
  return [ push( pop(result_stack), +top(result_stack) ),
           RS(stacks) ]

def _and(stacks):
  'substitui os dois valores do topo da pilha de trabalho pela comparação AND (do python) entre eles (1 ou 0)'
  result_stack = bin_op(WS(stacks), lambda x,y: x and y)
  return [ push( pop(result_stack), +top(result_stack) ),
           RS(stacks) ]

def _not(stacks):
  'substitui o valor do topo da pilha de trabalho pela operação NOT (do python) sobre ele (1 ou 0)'
  result_stack = una_op(WS(stacks), lambda x: not x)
  return [ push( pop(result_stack), +top(result_stack) ),
           RS(stacks) ]

def _empty(stacks):
  'substitui o valor do topo da pilha de trabalho por 1 se ela estiver vazia ou 0, caso contrário'
  return [ push( WS(stacks), +empty(WS(stacks)) ),
           RS(stacks) ]

def _swap(stacks):
  'troca os valores no topo da pilha de trabalho'
  return [ swap( WS(stacks) ),
           RS(stacks) ]

def _dup(stacks):
  'duplica o valor do topo da pilha de trabalho'
  ws = WS(stacks)
  return [ [] if empty(ws) else push(ws, top(ws)),
           RS(stacks) ]

def _roll(stacks):
  'move o n-ésimo valor por topo, onde n é o valor corrente do topo da pilha de trabalho'
  return [ roll( WS(stacks) ),
           RS(stacks) ]

def _move_ws_to_rs(stacks):
  'move o topo da pilha de trabalho para a pilha de resultados'
  return stacks[:] if empty( WS(stacks) )  \
         else [ pop( WS(stacks) ),
                push( RS(stacks), top(WS(stacks)) ) ]

def _move_rs_to_ws(stacks):
  'move o topo da pilha de resultados para a pilha de trabalho'
  return stacks[:] if empty( RS(stacks) )  \
         else [ push( WS(stacks), top(RS(stacks)) ),
                pop( RS(stacks) ) ]

def _copy_ws_to_rs(stacks):
  'copia o topo da pilha de trabalho para a pilha de resultados (sem consumir)'
  return stacks[:] if empty( WS(stacks) )  \
         else [ WS(stacks),
                push( RS(stacks), top(WS(stacks)) ) ]



primitives = {
  # The two Forth stacks (working and results) are represented by a list with two values.
  # Each Forth primitive receives the two stacks, modifying them if necessary.
  # -- dot: calls dot to show working stack top value (value is consumed)
  '.':      lambda stacks: [ dot(WS(stacks)), RS(stacks) ],
  # -- drop: drops element from working stack
  'drop':   lambda stacks: [ pop(WS(stacks)), RS(stacks) ],
  # -- rempty: pushes 1 to working stack if result stack is empty; 0, otherwise
  'rempty': lambda stacks: [ push(WS(stacks), int(empty(RS(stacks)))), RS(stacks) ],
  # -- sqrt: substitutes the top of the working stack value by its square root
  'sqrt':   lambda stacks: [ una_op(WS(stacks), sqrt), RS(stacks) ],
  # -- +: substitutes the two top values in working stack by their sum
  '+':      lambda stacks: [ bin_op(WS(stacks), lambda x,y: x+y), RS(stacks) ],

  ######################### RESPOSTAS DA QUESTÃO 3-6 #########################

  # -- disp: exibe na tela as pilhas de trabalho e de resultados (sem consumir)
  'disp': _disp,
  # -- -: realiza a subtração na pilha de resultados
  '-': _sub,
  # -- *: realiza a mutiplicação na pilha de resultados
  '*': _mul,
  # -- /: realiza a divisão na pilha de resultados
  '/': _div,
  # -- %: realiza a operação de resto (MOD) na pilha de resultados
  '%': _mod,
  # -- >: empilha 0 ou 1 de acordo com a saída da operação lógica MAIOR QUE
  '>': _greater_than,
  # -- <: empilha 0 ou 1 de acordo com a saída da operação lógica MAIOR QUE
  '<': _less_than,
  # -- =: empilha 1 se os dois valores do topo forem iguais ou 0 caso contrário
  '=': _equal,
  # -- or: empilha 0 ou 1 de acordo com a saída do operador relacional OR
  'or': _or,
  # -- and: empilha 0 ou 1 de acordo com a saída do operador relacional NOT
  'and': _and,
  # -- not: empilha 0 ou 1 de acordo com a saída do operador relacional NOT
  'not': _not,
  # -- empty: empilha 1 se pilha de trabalho está vazia (0, caso contrário)
  'empty': _empty,
  # -- swap: troca valores no topo da pilha de trabalho
  'swap': _swap,
  # -- dup: duplica o valor do topo da pilha de trabalho
  'dup': _dup,
  # -- roll: move o n-ésimo valor por topo (1 roll com 5 6 7 -> 6 5 7)
  'roll': _roll,
  # -- >r: move o topo da pilha de trabalho pra pilha de resultados
  '>r': _move_ws_to_rs,
  # -- r>: move o topo da pilha de resultados pra pilha de trabalho
  'r>': _move_rs_to_ws,
  # -- r@: copia o topo da pilha de trabalho pra pilha de resultados (sem consumir)
  'r@': _copy_ws_to_rs
}



def new_def(defs, word, seq):
  "Associates a word with a seq of Forth words provided by the user, using a Python's map called defs"
  defs[word] = seq
  return defs

def is_number(s):
  "Checks if s is a number"
  try:
    float(s)
    return True
  except ValueError:
    return False

def exec_begin_until_for(interpret):
  def exec_begin_until(proc_body, stacks, defs):
    if empty(proc_body): ## tratando o caso onde o corpo do `begin..until` não foi definido
      return (stacks, defs)

    n_stacks, n_defs = interpret(proc_body, stacks, defs)
    last_stacks = [ pop(WS(n_stacks)), RS(n_stacks) ]

    return (last_stacks, n_defs) if top(WS(n_stacks)) == 1 \
           else exec_begin_until(proc_body, last_stacks, n_defs)

  return exec_begin_until

def exec_if_else_then_for(interpret):
  def split_tokens_into_cond_if_else(tokens):
    seq1, exp_rest = split_val(tokens, 'else')
    seq2 = exp_rest[1:]
    return (seq1, seq2)

  def exec_if_else_then(proc_body, stacks, defs):
    if empty(proc_body): ## tratando o caso onde o corpo do `if..then` não foi definido
      return (stacks, defs)

    seq_if, seq_else = split_tokens_into_cond_if_else(proc_body)
    last_stacks = [ pop(WS(stacks)), RS(stacks) ]

    return interpret(seq_if, last_stacks, defs) if top(WS(stacks)) == 1 \
           else interpret(seq_else, last_stacks, defs)

  return exec_if_else_then

def interpret(tokens, stacks, defs):
  ## Interpret does not change stacks and defs if no tokens are provided
  if not tokens:
    return stacks, defs

  ## If number, stacks it
  elif is_number(tokens[0]):
    stacks[0] = push(stacks[0], float(tokens[0]))

  ## If :, get definition (everything between : and ;) and stores in defs
  elif tokens[0] == ':' and ';' in tokens:
    proc_body, tokens_remains = split_val(tokens[1:], ';')
    return interpret(tokens_remains[1:], stacks, new_def(defs, word= proc_body[0], seq= proc_body[1:]))

  ## If .", gets every word until " and display them
  elif tokens[0] == '."':
    string_body, tokens = split_val(tokens[1:], '"')
    print(' '.join(string_body), end='')

  ## If cr, emits \n
  elif tokens[0] == 'cr':
    print('\n', end='')

  ## If something in primitives, run it
  elif tokens[0] in primitives:
    stacks = primitives[tokens[0]](stacks)

  ######################### RESPOSTAS DA QUESTÃO 3-7 #########################

  elif tokens[0] == 'begin' and 'until' in tokens: ## não permite a definição multi line do `begin..until`
    proc_body, tokens_remains = split_val(tokens[1:], 'until')
    n_stacks, n_defs = exec_begin_until_for(interpret)(proc_body, stacks, defs)
    return interpret(tokens_remains[1:], n_stacks, n_defs)

  elif 'if' in tokens and 'then' in tokens: ## não permite a definição multi line do `if..else..then`
    proc_body, tokens_remains = split_val(tokens[1:], 'then')
    n_stacks, n_defs = exec_if_else_then_for(interpret)(proc_body, stacks, defs)
    return interpret(tokens_remains[1:], n_stacks, n_defs)

  ############################################################################

  ## If something in defs, run it
  elif tokens[0] in defs:
    stacks, defs = interpret(defs[tokens[0]], stacks, defs)

  ## Well, I dont understand you
  elif tokens[0]: ## apenas se não for vazio
    print('  Unknown word: [%s]' % tokens[0])

  return interpret(tokens[1:], stacks, defs)


def lforth_r(stacks, defs):
  try:
    uin = input('\n$ ')
    if uin == 'quit' or uin == 'q':
      return
    n_stacks, n_defs = interpret(uin.split(' '), stacks, defs)
    return lforth_r(n_stacks, n_defs)
  except (KeyboardInterrupt, EOFError) as err:
    return

def lforth():
  print('ulikeForth by Micael Levi. Type "quit" or "q" to exit')
  lforth_r(stacks = [[], []], defs = dict([]))



if __name__ == '__main__':
  lforth()
