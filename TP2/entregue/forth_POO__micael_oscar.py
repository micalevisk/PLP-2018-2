## Forth OO - usando Python 3.6

from math import sqrt
from functools import reduce


################################################################################
class Utils(object):
  """
  Biblioteca de métodos estáticos utilitários/helpers.
  """

  @staticmethod
  def is_number(s):
    'Checks if s is a number.'
    try:
      float(s)
      return True
    except ValueError:
      return False

  @staticmethod
  def split_val(l, v):
    'Separa a lista `l` em 2 partes, a primeira contém os elementos observados até a primeira ocorrência do valor `v`.'
    lanteriores = []
    lresto = []
    for i, e in enumerate(l):
      if e == v:
        lresto = l[i:]
        break
      lanteriores.append(e)
    return (lanteriores, lresto)


################################################################################
class Stack(object):
  """
  Define uma estrutura com operações relativas à uma pilha (política FILO).
  """

  def __init__(self):
    self.__valores = []

  def __repr__(self):
    return '{}({})'.format(self.__class__.__name__, self.__valores)
  def __str__(self):
    return str(self.__valores)
  def __len__(self):
    return len(self.__valores)
  def __eq__(self, stack2):
    return self.__valores == stack2 if isinstance(stack2, list)  \
           else self.__valores == stack2.__valores

  def empty(self):
    'Retorna `True` se a pilha estiver vazia. `False` caso contrário.'
    return len(self) == 0

  def push(self, e):
    'Insere um elemento no topo da pilha. Retorna a própria pilha.'
    self.__valores.append(e)
    return self

  def pop(self):
    'Retira o elemento do topo da pilha, se ela não estiver vazia. Retorna a própria pilha.'
    if not self.empty():
      self.__valores.pop()
    return self

  def top(self):
    'Retorna o topo da pilha se ela não estiver vazia. `None` caso contrário.'
    return self.__valores[-1] if not self.empty() else None

  def transfer_to(self, other_stack):
    'Desempilha todos os valores em uma outra pilha.'
    ## repush
    while not self.empty():
      other_stack.push( self.top() )
      self.pop()


################################################################################
class StackForth(Stack):
  """
  Define uma pilha especializada para ser usada no interpretador Forth.
  """

  def top_value(self):
    'Retorna a pilha serializada ou a string `?` se a pilha estiver vazia'
    return '?' if self.empty() else '%f' % self.top() if self.top()%1 != 0 else '%d' % self.top()

  def bin_op(self, fb):
    'Substitui os dois valores do topo pela aplicação da função `fb`. Retorna a própria pilha.'
    if self.empty():
      return self
    if not len(self) > 1:
      raise ValueError('A pilha deve ter mais de 1 valor, tem %s.' % len(self))

    op1, op2 = self.top(), self.pop().top()
    self.pop().push( fb(op1, op2) )

    return self

  def una_op(self, fu):
    'Substitui o valor do topo pela aplicação da função `fu`. Retorna a própria pilha.'
    if self.empty():
      return self

    op = self.top()
    self.pop().push( fu(op) )

    return self

  def swap(self):
    'Troca os dois valores do topo da pilha. Retorna a própria pilha.'
    if len(self) < 2:
      raise ValueError('A pilha deve ter mais de 1 valor, tem %s' % len(self))

    topo_anterior, novo_topo = self.top(), self.pop().top()
    self.pop().push(topo_anterior).push(novo_topo)

    return self


################################################################################
class InterpreterCore(object):
  """
  Fornece as primitivas do núcleo da linguagem Forth e atua sobre as duas pilhas internas.
  """

  def __init__(self):
    self.__ws = StackForth()
    self.__rs = StackForth()

  def push_ws(self, value):
    'Empilha `value` (não None) na pilha de trabalho.'
    if value != None:
      self.__ws.push(value)

  def pop_ws(self):
    'Desempilha a pilha de trabalho, retornando o valor desempilhado. Ou None.'
    valor_desempilhado = self.__ws.top()
    self.__ws.pop()
    return valor_desempilhado

  def __ws_bin_op_rint(self, op):
    'Realiza uma operação binária sobre a pilha de trabalho e empilha o resultado como inteiro (1 ou 0).'
    if len(self.__ws) > 1:
      self.__ws.bin_op(op)
      resultado_como_int = int( self.__ws.top() )
      self.__ws.pop().push(resultado_como_int)


  def _disp(self):
    'Exibe na tela as duas pilhas, sem consumi-las.'
    print('working stack =', self.__ws)
    print('result  stack =', self.__rs)

  def _dot(self):
    'Desempilha a pilha de trabalho e exibe na tela.'
    print(self.__ws.top_value(), end='')
    self.__ws.pop()

  def _cr(self):
    'Saltar uma linha (imprimir o caractere de Line Feed).'
    print('\n', end='')

  ## ======================== operadores aritméticos ======================== ##

  def _add(self):
    'Realiza a adição dos dois valores do topo da pilha de trabalho.'
    self.__ws.bin_op(lambda x,y: x+y)

  def _sub(self):
    'Realiza a subtração dos dois valores do topo da pilha de trabalho.'
    self.__ws.bin_op(lambda x,y: x-y)

  def _mul(self):
    'Realiza a multiplicação dos dois valores do topo da pilha de trabalho.'
    self.__ws.bin_op(lambda x,y: x*y)

  def _div(self):
    'Realiza a divisão dos dois valores do topo da pilha de trabalho.'
    self.__ws.bin_op(lambda x,y: x/y)

  def _mod(self):
    'Realiza o módulo dos dois valores do topo da pilha de trabalho.'
    self.__ws.bin_op(lambda x,y: x%y)

  def _sqrt(self):
    'Substitui o topo da pilha de trabalho pela sua raiz quadrada.'
    self.__ws.una_op(lambda x: sqrt(x))

  ## ======================== operadores lógicos e relacionais ======================== ##

  def _greater_than(self):
    'Substitui os dois valores do topo da pilha de trabalho pela comparação MAIOR QUE entre eles (1 ou 0).'
    self.__ws_bin_op_rint(lambda x,y: y>x) ## o topo é o segundo operando

  def _less_than(self): ## não trata divisões por 0
    'Substitui os dois valores do topo da pilha de trabalho pela comparação MENOR QUE entre eles (1 ou 0).'
    self.__ws_bin_op_rint(lambda x,y: y<x) ## o topo é o segundo operando

  def _equal(self):
    'Substitui os dois valores do topo da pilha de trabalho pela comparação IGUAL entre eles (1 ou 0).'
    self.__ws_bin_op_rint(lambda x,y: x==y)

  def _or(self):
    'Substitui os dois valores do topo da pilha de trabalho pela comparação OR (do python) entre eles (1 ou 0).'
    self.__ws_bin_op_rint(lambda x,y: x or y)

  def _and(self):
    'Substitui os dois valores do topo da pilha de trabalho pela comparação OR (do python) entre eles (1 ou 0).'
    self.__ws_bin_op_rint(lambda x,y: x and y)

  def _not(self):
    'Substitui o valor do topo da pilha de trabalho pela operação NOT (do python) sobre ele (1 ou 0)'
    if not self.__ws.empty():
      self.__ws.una_op(lambda x: not x)
      resultado_como_int = int( self.__ws.top() )
      self.__ws.pop().push(resultado_como_int)

  ## ======================== outras primitivas ======================== ##

  def _empty(self):
    'Empilha 1 na pilha de trabalho se ela estiver vazia (0, caso contrário).'
    self.__ws.push( +self.__ws.empty() )

  def _rempty(self):
    'Empilha 1 na pilha de trabalho se a pilha de resultados estiver vazia (0, caso contrário).'
    self.__ws.push( +self.__rs.empty() )

  def _drop(self):
    'Desempilha o valor do topo da pilha de trabalho, retornando-o.'
    topo_antigo = self.__ws.top()
    self.__ws.pop()
    return topo_antigo

  def _swap(self):
    'Troca os dois valores do topo da pilha de trabalho.'
    self.__ws.swap()

  def _dup(self):
    'Duplica o valor do topo da pilha de trabalho.'
    if not self.__ws.empty():
      self.__ws.push( self.__ws.top() )

  def _rot(self):
    'Rotaciona top-3 valores da pilha de trabalho. O mesmo que `swap >r swap r>`.'
    if len(self.__ws) > 2:
      self._swap()
      antigo_topo = self.__ws.top()
      self.__ws.pop()
      self._swap()
      self.__ws.push(antigo_topo)

  def _roll(self):
    'Move o n-ésimo valor da pilha de trabalho para o topo, onde `n` é o topo corrente.'
    if self.__ws.empty() or self.__ws.top() >= len(self.__ws) - 1:
      return

    n = self.__ws.top()
    self.__ws.pop()

    temp_stack = StackForth()
    while n > 0:
      n -= 1
      temp_stack.push( self.__ws.top() )
      self.__ws.pop()

    novo_topo = self.__ws.top()
    self.__ws.pop()

    temp_stack.transfer_to(self.__ws)
    self.__ws.push(novo_topo)

  def _pick(self):
    'Copia o n-ésimo valor da pilha de trabalho para o topo, onde `n` é o topo corrente. O mesmo que `roll dup`.'
    if self.__ws.empty() or self.__ws.top() >= len(self.__ws):
      return

    self._roll()
    self._dup()

  def _move_ws_to_rs(self):
    'Move o topo da pilha de trabalho para a pilha de resultados.'
    if not self.__ws.empty():
      self.__rs.push( self.__ws.top() )
      self.__ws.pop()

  def _move_rs_to_ws(self):
    'Move o topo da pilha de resultados para a pilha de trabalho.'
    if not self.__rs.empty():
      self.__ws.push( self.__rs.top() )
      self.__rs.pop()

  def _copy_ws_to_rs(self):
    'Copia o topo da pilha de trabalho pra pilha de resultados.'
    if not self.__ws.empty():
      self.__rs.push( self.__ws.top() )


################################################################################
class Interpreter(InterpreterCore):
  def __init__(self):
    InterpreterCore.__init__(self)

    self.defs = dict([])

    self.primitives = {
      'disp': self._disp, '.': self._dot, 'cr': self._cr,
      'sqrt': self._sqrt, '+': self._add, '-': self._sub, '*': self._mul, '/': self._div, '%': self._mod,
      '>': self._greater_than, '<': self._less_than, '=': self._equal,
      'or': self._or, 'and': self._and, 'not': self._not,
      'empty': self._empty, 'rempty': self._rempty, 'drop': self._drop, 'swap': self._swap, 'dup': self._dup,
      'rot': self._rot, 'roll': self._roll, 'pick': self._pick,
      '>r': self._move_ws_to_rs, 'r>': self._move_rs_to_ws, 'r@': self._copy_ws_to_rs
    }

    self.complex_primitives = { ## recebem tokens, i.e., possuem um "corpo"
      ':': self._start_new_word,
      '."': self._start_string,
      'if': self._start_if_else_then,
      'begin': self._start_begin_until,
    }


  @staticmethod
  def calcular_prox_token(proc_body):
    'Para computar o índice do próximo token a ser avaliado, dado uma primitiva com "corpo" (token de início e fim).'
    return len(proc_body) + 1

  @staticmethod
  def tokenizer(string):
    return string.split(' ')


  def ws_top_to_bool(self):
    'Retorna a representação booleana (0 ou 1) do topo corrente da pilha de trabalho. Ou `None` se estiver vazia.'
    self._dup()
    self._not()
    self._not()
    return self.pop_ws()


  def _start_new_word(self, body):
    'Tratar a palavra `:` ... `;`'
    if ';' not in body:
      return 0

    proc_body, _ = Utils.split_val(body, ';')

    if proc_body:
      new_word = proc_body[0]
      if new_word in self.primitives or new_word in self.complex_primitives:
        print('  ERROR: the word `%s` can not be replaced' % new_word)

      elif new_word.isdigit():
        print('  ERROR: the new word (`%s`) can not be a digit' % new_word)

      else:
        self.defs[new_word] = proc_body[1:] ## new_def

    return Interpreter.calcular_prox_token(proc_body)

  def _start_string(self, body):
    'Tratar a palavra `."` ... `"`'
    if '"' not in body:
      return 0

    proc_body, _ = Utils.split_val(body, '"')

    print(' '.join(proc_body), end='')

    return Interpreter.calcular_prox_token(proc_body)

  def _start_if_else_then(self, body):
    'Tratar a palavra `if` ... [`else` ...] `then`'
    if 'then' not in body:
      return 0

    proc_body, _ = Utils.split_val(body, 'then')

    seq_if, exp_rest = Utils.split_val(proc_body, 'else')
    seq_else = exp_rest[1:]

    cond = self.ws_top_to_bool()
    self.pop_ws() ## consumindo a condição
    self.interpretar(seq_if if cond else seq_else)

    return Interpreter.calcular_prox_token(proc_body)

  def _start_begin_until(self, body):
    'Tratar a palavra `begin` ... `until`'
    proc_body, _ = Utils.split_val(body, 'until')

    while True:
      self.interpretar(proc_body)
      cond = self.ws_top_to_bool()
      self.pop_ws() ## consumindo a condição
      if cond:
        break

    return Interpreter.calcular_prox_token(proc_body)


  def interpretar(self, tokens):
    if not tokens:
      pass

    i = 0
    while i < len(tokens):
      token = tokens[i]
      i += 1

      if Utils.is_number(token):
        self.push_ws( float(token) )

      elif token in self.primitives:
        self.primitives[token]()

      elif token in self.complex_primitives:
        idx_next_token = self.complex_primitives[token](tokens[i:])
        i += idx_next_token

      elif token in self.defs:
        self.interpretar(self.defs[token]) ## chamada recursiva

      elif token:
        print('  Unknown word: [%s]' % token)

  def run_loop(self):
    'Executar o interpretador em Read-Eval-Print Loop.'
    print('ulikeForth by Micael Levi. Type "quit" or "q" to exit')

    while True:
      try:
        uin = input('\n$ ') #raw_input('\n$ ')
        if uin == 'quit' or uin == 'q':
          return
        self.interpretar( Interpreter.tokenizer(uin) )
      except (KeyboardInterrupt, EOFError) as err:
        return




##################################
if __name__ == '__main__':
  lforth = Interpreter()
  lforth.run_loop()
