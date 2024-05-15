class ape:

  def __init__(self, address, operator, operand):
    self.address = address
    self.operator = operator
    self.operand = operand


def gen_instr(op, oprnd):
  global instr_address

  temp = ape(instr_address, op, oprnd)

  instr_table.append(temp)
  instr_address += 1


def back_patch(jump_addr):
  temp_instr_address = jumpstack.pop_jumpstack()
  for i in instr_table:
    if i.address == temp_instr_address:
      i.operand = jump_addr


class symbol:

  def __init__(self, identifier, memoryLocation, type):
    self.identifier = identifier
    self.memoryLocation = memoryLocation
    self.type = type


class Stack:

  def __init__(self):
    self.items = []

  def push_jumpstack(self, item):
    self.items.append(item)

  def pop_jumpstack(self):
    if not self.is_empty():
      return self.items.pop()

  def peek(self):
    if not self.is_empty():
      return self.items[-1]

  def is_empty(self):
    return len(self.items) == 0

  def size(self):
    return len(self.items)


jumpstack = Stack()

switch = False
check = False
save = 0
tempType = " "
symbolCheck = False

idTable = []


def get_address(id):
  for i in idTable:
    if i[0] == id:
      temp_oprnd = i[1]
  return temp_oprnd


def push_symbol(id, op, type):
  symbolTable.append([id, op, type])


# c is a global variable and is used to access the indices of file.
# Everytime c is incremented, that means we are moving onto the next character.
c = 0

# x is used to traverse through tokenLexemeList
x = 0

# Accepting states for id_DFSM is 2 and 4.
# Accepting states for num_DFSM is 2 (integer) and 4 (real)
# Invalid state is 3
state = 1

# The switch for the syntax analyzer. If "y" then print with production rules. If "n" then don't print with production rules.
syntaxAnalyzerSwitch = ""

# output is a global variable to add characters into it in different functions.
output = ""

currentToken = ""
currentLexeme = ""

# tokenLexemeList is a vector that stores the tokens and lexemes to output to the text file.
tokenLexemeList = []

# Holds tokens, lexemes, and production rules.
masterList = []

# Holds the address and current operand
curr_op = 5000
instr_address = 1

# Array to store the instructions
instr_table = []

# Array to store the symbol table
symbolTable = []

op = {
  '=', '+', '-', '/', '<=', '>=', '>', '<', '+=', '-=', '/=', '%=', '%', '==',
  '!', '*', '*=', '**', '<<', '>>', 'or', '!=', '::', '"'
}

kw = {
  'for', 'For', 'while', 'While', 'do while', 'Do while', 'endwhile',
  'Endwhile', 'if', 'If', 'else', 'Else', 'else if', 'Else if', 'int', 'Int',
  'double', 'Double', 'float', 'Float', 'bool', 'Bool', 'char', 'Char',
  'string', 'String', 'continue', 'Continue', 'break', 'Break', 'return',
  'Return', 'function', 'Function', 'string', 'String', 'main', 'Main', 'put',
  'Put', 'get', 'Get', 'in', 'In', 'using', 'Using', 'std', 'fi', 'Fi'
}

sp = {';', ',', '(', ')', '{', '}', '[', ']', ':', '#'}

comments = {'[*', '*]', '/*', '*/'}

# DFSM Table for Identifiers based on the regular expression: l(l|d|'_')*
id_table = {1: [2, 3, 3], 2: [4, 4, 4], 3: [3, 3, 3], 4: [4, 4, 4]}

# DFSM Table for Integers/Real based on the regular expressions:
# Integers: d+
# Real: d+.d+
num_table = {1: [2, 3], 2: [2, 4], 3: [3, 3], 4: [4, 3]}


def LA(file):
  global c
  global tokenLexemeList

  if (file[c] == " "
      or file[c] == '\n'):  # Check for white spaces or newlines, ignore them
    c += 1
    return
  elif (file[c] == "["):
    if (file[c] + file[c + 1]
        in comments):  # Check for Rat23S comments, ignore them
      c += 2
      while not (file[c] + file[c + 1] in comments):
        c += 1
      c += 2
      return
  elif (file[c] == "_" or file[c] == "."):  # Check for underscores
    c += 1
    while not (file[c] == " " or file[c] == "\n"):
      c += 1
    return
  elif file[c] == '"':  # Check for literal strings, ignore them
    c += 1
    while not (file[c] == '"'):
      c += 1
    c += 1
    return
  elif (file[c] in sp):  # Check for separators
    tokenLexemeList.append(["SEPARATOR", file[c]])
    c += 1
  elif (file[c] in op):  # Check for operators
    if ((file[c] + file[c + 1]) in op):
      tokenLexemeList.append(["OPERATOR", file[c] + file[c + 1]])
      c += 2
    else:
      tokenLexemeList.append(["OPERATOR", file[c]])
      c += 1
  elif (file[c].isalpha()):  # Check for identifiers and keywords
    id_DFSM(file)
    if state == 3:
      return
    if output in kw:
      tokenLexemeList.append(["KEYWORD", output])
      return
    tokenLexemeList.append(["IDENTIFIER", output])
  elif (file[c].isdigit()):  # Check for integers and real numbers
    num_DFSM(file)
    if state == 2:
      tokenLexemeList.append(["INTEGER", output])
    elif state == 4:
      tokenLexemeList.append(["REAL", output])
    elif state == 3:
      return


def id_DFSM(file):
  global c
  global output
  global state
  state = 1
  output = ""

  while (state == 2 or state == 4 or state == 1 or state == 3):
    if c >= len(file):
      return
    if file[c].isalpha():
      state = id_table[state][0]  #letters
    elif file[c].isdigit():
      state = id_table[state][1]  #digits
    elif file[c] == "_":
      state = id_table[state][2]  #underscores
    elif file[c] in sp or file[c] in op or file[c] == " " or file[c] == "\n":
      return
    else:
      state = 3

    if (state == 2 or state == 4):
      output += str(file[c])
      c += 1
    elif state == 3:
      c += 1


def num_DFSM(file):
  global c
  global output
  global state
  state = 1
  output = ""

  while (state == 2 or state == 4 or state == 1):
    if c >= len(file):
      return
    if file[c].isdigit():
      state = num_table[state][0]  #digits
    elif file[c] == ".":
      state = num_table[state][1]  #dot
    else:
      return

    if state == 2:
      output += str(file[c])
      c += 1
    elif state == 4:
      output += str(file[c])
      c += 1
    elif state == 3:
      c += 1
      return


# Start of Syntax Analyzer code.


def Identifier():
  global isPrint
  global curr_op
  global save

  duplicate = False

  if currentToken == "IDENTIFIER":
    save = currentLexeme
    for i in idTable:
      if i[0] == save:
        duplicate = True

    if duplicate == False:
      idTable.append([currentLexeme, curr_op])

    masterList.append("<Identifier> -> " + "'" + currentLexeme + "'")
    if switch == False:
      if check == True:
        gen_instr("POPM", get_address(save))
        if duplicate == False:
          curr_op += 1
      else:
        gen_instr("PUSHM", get_address(save))
        if duplicate == False:
          curr_op += 1
    lexer()
  elif currentToken == "KEYWORD":
    masterList.append("Syntax Error: " + currentLexeme + " is a keyword.")
    syntaxError()
  else:
    masterList.append("Syntax Error: identifier expected.")
    syntaxError()


def Integer():
  if currentToken == "INTEGER":
    masterList.append("<Integer> -> " + currentLexeme)
    gen_instr("PUSHI", currentLexeme)
    lexer()
  else:
    masterList.append("Syntax Error: integer was expected.")
    syntaxError()


def Real():
  if currentToken == "REAL":
    masterList.append("<Real> -> " + currentLexeme)
    lexer()
  else:
    masterList.append("Syntax Error: real was expected.")
    syntaxError()


def lexer():
  global x
  global masterList
  global currentToken
  global currentLexeme
  global nextToken
  global nextLexeme
  global prevToken
  global prevLexeme

  if x < (len(tokenLexemeList) - 1):
    x += 1

  prevToken = tokenLexemeList[x - 1][0]
  prevLexeme = tokenLexemeList[x - 1][1]

  currentToken = tokenLexemeList[x][0]
  currentLexeme = tokenLexemeList[x][1]

  if x < (len(tokenLexemeList) - 2):
    nextToken = tokenLexemeList[x + 1][0]
    nextLexeme = tokenLexemeList[x + 1][1]

  masterList.append("\n========| Token: " + currentToken +
                    " |========| Lexeme: " + currentLexeme + " |========\n")


# R1
def Rat23S():
  masterList.append("\n========| Token: " + currentToken +
                    " |========| Lexeme: " + currentLexeme + " |========\n")
  masterList.append(
    "<Rat23S> -> <Opt Function Definitions> # <Opt Declaration List> # <Statement List>"
  )
  OptFunctionDefinitions()
  if currentLexeme == "#":
    lexer()
  else:
    masterList.append("Syntax Error: '#' was expected.")
    syntaxError()
  OptDeclarationList()
  if currentLexeme == "#":
    lexer()
  else:
    masterList.append("Syntax Error: '#' was expected.")
    syntaxError()
  StatementList()


# R2
def OptFunctionDefinitions():
  if currentLexeme == "function":
    masterList.append("<Opt Function Definitions> -> <Function Definitions>")
    FunctionDefinition()
  else:
    masterList.append("<Opt Function Definitions> -> <Empty>")


# R3
def FunctionDefinition():
  masterList.append("<Function Definitions> -> <Function> <Function Prime>")
  Function()
  FunctionPrime()


def FunctionPrime():
  if currentLexeme == "function":
    masterList.append("<Function Prime> -> <Function Definitions>")
    FunctionDefinition()
  else:
    masterList.append("<Function Prime> -> <Empty>")


# R4
def Function():
  global check
  global switch

  masterList.append(
    "<Function> -> function <Identifier> ( <Opt Parameter List> ) <Opt Declaration List> <Body>"
  )
  if currentLexeme == "function":
    lexer()
  else:
    masterList.append("Syntax Error: 'function' Expected.")
    syntaxError()
  duplicate = False
  for i in idTable:
    if i[0] == currentLexeme:
      duplicate = True
  if duplicate == False:
    push_symbol(currentLexeme, curr_op, "function")
  Identifier()
  if currentLexeme == "(":
    lexer()
  else:
    masterList.append("Syntax Error: '(' Expected.")
    syntaxError()
  OptParameterList()
  if currentLexeme == ")":
    lexer()
  else:
    masterList.append("Syntax Error: ')' Expected.")
    syntaxError()
  OptDeclarationList()
  Body()


# R5
def OptParameterList():
  if currentLexeme != ")":
    masterList.append("<Opt Parameter List> -> <Parameter List>")
    ParameterList()
  else:
    masterList.append("<Opt Parameter List> -> <Empty>")


# R6
def ParameterList():
  masterList.append("<Parameter List> -> <Parameter> <Parameter List Prime>")
  Parameter()
  ParameterListPrime()


def ParameterListPrime():
  if currentLexeme == ",":
    masterList.append("<Parameter List Prime> -> <Parameter List>")
    lexer()
    ParameterList()
  else:
    masterList.append("<Parameter List Prime> -> <Empty>")


# R7
def Parameter():
  global symbolCheck

  masterList.append("<Parameter> -> <IDs> <Qualifier>")

  tempID = currentLexeme
  tempOP = curr_op
  symbolCheck = False
  IDs()
  symbolCheck = True
  Qualifier()

  duplicate = False
  for i in idTable:
    if i[0] == symbolTable:
      duplicate = True
  if duplicate == False:
    push_symbol(tempID, tempOP, tempType)
  duplicate = False


# R8
def Qualifier():
  global tempType

  if currentLexeme == "int":
    masterList.append("<Qualifier> -> int")
    tempType = "integer"
    lexer()
  elif currentLexeme == "bool":
    masterList.append("<Qualifier> -> bool")
    tempType = "bool"
    lexer()
  elif currentLexeme == "real":
    masterList.append("<Qualifier> -> real")
    tempType = "real"
    lexer()
  else:
    masterList.append("<Qualifier> -> <Empty>")


# R9
def Body():
  masterList.append("<Body> -> { <Statement List> }")
  if currentLexeme == "{":
    lexer()
  else:
    masterList.append("Syntax Error: '{' Expected.")
    syntaxError()
  StatementList()
  if currentLexeme == "}":
    lexer()
  else:
    masterList.append("Syntax Error: '}' Expected.")
    syntaxError()


# R10
def OptDeclarationList():
  if currentLexeme == "int" or currentLexeme == "bool" or currentLexeme == "real":
    masterList.append("<Opt Declaration List> -> <Declaration List>")
    switch = True
    DeclarationList()
    switch = False
  else:
    masterList.append("<Opt Declaration List> -> <Empty>")


# R11
def DeclarationList():
  masterList.append(
    "<Declaration List> -> <Declaration> ; <Declaration List Prime>")
  Declaration()
  if currentLexeme == ";":
    lexer()
  DeclarationListPrime()


def DeclarationListPrime():
  if currentLexeme == "int" or currentLexeme == "bool" or currentLexeme == "real":
    masterList.append("<Declaration List Prime> -> <Declaration List>")
    lexer()
    DeclarationList()
  else:
    masterList.append("<Declaration List Prime> -> <Empty>")


# R12
def Declaration():
  masterList.append("<Declaration> -> <Qualifier> <IDs>")
  Qualifier()
  IDs()


# R13
def IDs():
  global tempType
  global symbolCheck

  masterList.append("<IDs> -> <Identifier> <IDs Prime>")

  tempOP = curr_op
  duplicate = False
  if symbolCheck == True:
    for i in symbolTable:
      if i[0] == currentLexeme:
        duplicate = True
    for i in idTable:
      if i[0] == currentLexeme:
        tempOP = i[1]
    if duplicate == False:
      push_symbol(currentLexeme, tempOP, tempType)
  duplicate = False
  Identifier()
  IDsPrime()


def IDsPrime():
  if currentLexeme == ",":
    masterList.append("<IDs Prime> -> <IDs>")
    lexer()
    IDs()
  else:
    masterList.append("<IDs Prime> -> <Empty>")


# R14
def StatementList():
  masterList.append("<Statement List> -> <Statement> <Statement List Prime>")
  Statement()
  StatementListPrime()


def StatementListPrime():
  if currentLexeme == "{" or currentToken == "IDENTIFIER" or currentLexeme == "if" or currentLexeme == "return" or currentLexeme == "put" or currentLexeme == "get" or currentLexeme == "while":
    masterList.append("<Statement List Prime> -> <Statement List>")
    StatementList()
  else:
    masterList.append("<Statement List Prime> -> <Empty>")


# R15
def Statement():
  if currentLexeme == "{":
    masterList.append("<Statement> -> <Compound>")
    Compound()
  elif currentLexeme == "if":
    masterList.append("<Statement> -> <If>")
    If()
  elif currentLexeme == "return":
    masterList.append("<Statement> -> <Return>")
    Return()
  elif currentLexeme == "put":
    masterList.append("<Statement> -> <Print>")
    Print()
  elif currentLexeme == "get":
    masterList.append("<Statement> -> <Scan>")
    Scan()
  elif currentLexeme == "while":
    masterList.append("<Statement> -> <While>")
    While()
  elif currentToken == "IDENTIFIER":
    masterList.append("<Statement> -> <Assign>")
    Assign()


# R16
def Compound():
  masterList.append("<Compound> -> { <Statement List> }")
  if currentLexeme == "{":
    lexer()
  else:
    masterList.append("Syntax Error: '{' was expected.")
    syntaxError()
  StatementList()
  if currentLexeme == "}":
    lexer()
  else:
    masterList.append("Syntax Error: '}' was expected.")
    syntaxError()


# R17
def Assign():
  global switch
  global check
  global curr_op
  global save

  save2 = currentLexeme

  masterList.append("<Assign> -> <Identifier> = <Expression> ;")
  switch = True
  Identifier()
  switch = False
  if currentLexeme == '=':
    lexer()
  else:
    masterList.append("Syntax Error: '=' was expected.")
    syntaxError()
  Expression()
  gen_instr("POPM", get_address(save2))
  curr_op += 1
  if currentLexeme == ';':
    lexer()
  else:
    masterList.append("Syntax Error: ';' was expected.")
    syntaxError()


# R18
def If():
  masterList.append("<If> -> if ( <Condition> ) <Statement> <If Prime>")
  if currentLexeme == "if":
    lexer()
  else:
    masterList.append("Syntax Error: 'if' was expected.")
    syntaxError()
  if currentLexeme == "(":
    lexer()
  else:
    masterList.append("Syntax Error: '(' was expected.")
    syntaxError()
  Condition()
  if currentLexeme == ")":
    lexer()
  else:
    masterList.append("Syntax Error: ')' was expected.")
    syntaxError()
  Statement()
  back_patch(instr_address)
  IfPrime()


def IfPrime():
  if currentLexeme == "fi":
    gen_instr("LABEL", " ")
    masterList.append("<If Prime> -> fi")
    lexer()
  elif currentLexeme == "else":
    masterList.append("<If Prime> -> else <Statement> fi")
    lexer()
    Statement()
    if currentLexeme == "fi":
      gen_instr("LABEL", " ")
      lexer()
  else:
    masterList.append("Syntax Error: 'else' or 'fi' was expected.")
    syntaxError()


# R19
def Return():
  masterList.append("<Return> -> return <Return Prime>")
  if currentLexeme == "return":
    lexer()
  else:
    masterList.append("Syntax Error: 'return' was expected.")
    syntaxError()
  ReturnPrime()


def ReturnPrime():
  if currentLexeme == ";":
    masterList.append("<Return Prime> -> ;")
    lexer()
  else:
    masterList.append("<Return Prime> -> <Expression> ;")
    Expression()
    lexer()


# R20
def Print():
  global check
  global switch

  masterList.append("<Print> -> put ( <Expression> ) ; ")
  if currentLexeme == 'put':
    lexer()
  else:
    masterList.append("Syntax Error: 'put' was expected.")
    syntaxError()
  if currentLexeme == '(':
    lexer()
  else:
    masterList.append("Syntax Error: '(' was expected.")
    syntaxError()
  Expression()
  if currentLexeme == ')':
    lexer()
  else:
    masterList.append("Syntax Error: ')' was expected.")
    syntaxError()
  if currentLexeme == ';':
    lexer()
  else:
    masterList.append("Syntax Error: ';' was expected.")
    syntaxError()
  gen_instr("OUT", " ")


# R21
def Scan():
  global check
  global switch

  check = True
  masterList.append("<Scan> -> get (<IDs>) ;")
  if currentLexeme == 'get':
    gen_instr("IN", " ")
    lexer()
  else:
    masterList.append("Syntax Error: 'get' was expected.")
    syntaxError()
  if currentLexeme == '(':
    lexer()
    IDs()
  else:
    masterList.append("Syntax Error: '(' was expected.")
    syntaxError()
  if currentLexeme == ')':
    lexer()
  else:
    masterList.append("Syntax Error: ')' was expected.")
    syntaxError()
  if currentLexeme == ';':
    lexer()
  else:
    masterList.append("Syntax Error: ';' was expected.")
    syntaxError()
  check = False


# R22
def While():
  masterList.append("<While> -> while (<Condition>) <Statement> endwhile")
  if currentLexeme == 'while':
    temp_instr_address = instr_address
    gen_instr("LABEL", " ")
    lexer()
  else:
    masterList.append("Syntax Error: 'while' was expected.")
    syntaxError()
  if currentLexeme == '(':
    lexer()
  else:
    masterList.append("Syntax Error: '(' wase expected.")
    syntaxError()
  Condition()
  if currentLexeme == ')':
    lexer()
  else:
    masterList.append("Syntax Error: ')' was expected.")
    syntaxError()
  Statement()
  gen_instr("JMP", temp_instr_address)
  back_patch(instr_address)
  if currentLexeme == 'endwhile':
    lexer()
  else:
    masterList.append("Syntax Error: 'endwhile' was expected.")
    syntaxError()


# R23
def Condition():
  masterList.append("<Condition> -> <Expression> <Relop> <Expression>")
  Expression()
  operation = currentLexeme
  Relop()
  Expression()
  if operation == '==':
    gen_instr("EQU", " ")
    jumpstack.push_jumpstack(instr_address)
    gen_instr("JMPZ", " ")
  elif operation == '!=':
    gen_instr("NEQ", " ")
    jumpstack.push_jumpstack(instr_address)
    gen_instr("JMPZ", " ")
  elif operation == '>':
    gen_instr("GRT", " ")
    jumpstack.push_jumpstack(instr_address)
    gen_instr("JMPZ", " ")
  elif operation == '<':
    gen_instr("LES", " ")
    jumpstack.push_jumpstack(instr_address)
    gen_instr("JMPZ", " ")
  elif operation == '<=':
    gen_instr("LEQ", " ")
    jumpstack.push_jumpstack(instr_address)
    gen_instr("JMPZ", " ")
  elif operation == '>=':
    gen_instr("GEQ", " ")
    jumpstack.push_jumpstack(instr_address)
    gen_instr("JMPZ", " ")


# R24
def Relop():
  if currentLexeme == '==':
    masterList.append("<Relop> -> ==")
    lexer()
  elif currentLexeme == '!=':
    masterList.append("<Relop> -> !=")
    lexer()
  elif currentLexeme == '>':
    masterList.append("<Relop> -> >")
    lexer()
  elif currentLexeme == '<':
    masterList.append("<Relop> -> <")
    lexer()
  elif currentLexeme == '<=':
    masterList.append("<Relop> -> <=")
    lexer()
  elif currentLexeme == '>=':
    masterList.append("<Relop> -> >=")
    lexer()
  else:
    masterList.append("Syntax Error: EXPECTED ==|!=|>|<|<=|>=")
    syntaxError()


# R25
def Expression():
  masterList.append("<Expression> -> <Term> <Expression Prime>")
  Term()
  ExpressionPrime()


def ExpressionPrime():
  if currentLexeme == '+':
    masterList.append("<Expression Prime> -> + <Term> <Expression Prime>")
    # sum = prevLexeme + nextLexeme
    lexer()
    Term()
    gen_instr("ADD", " ")
    # jumpstack.pop_jumpstack()
    # jumpstack.pop_jumpstack()
    # jumpstack.push_jumpstack(sum)
    ExpressionPrime()
  elif currentLexeme == '-':
    masterList.append("<Expression Prime> -> - <Term> <Expression Prime>")
    lexer()
    Term()
    gen_instr("SUB", " ")
    ExpressionPrime()
  else:
    masterList.append("<Expression Prime> -> <Empty>")


# R26
def Term():
  masterList.append("<Term> -> <Factor> <Term Prime>")
  Factor()
  TermPrime()


def TermPrime():
  if currentLexeme == "*":
    masterList.append("<Term Prime> -> * <Factor> <Term Prime>")
    lexer()
    Factor()
    gen_instr("MUL", " ")
    TermPrime()
  elif currentLexeme == "/":
    masterList.append("<Term Prime> -> / <Factor> <Term Prime>")
    lexer()
    Factor()
    gen_instr("DIV", " ")
    TermPrime()
  else:
    masterList.append("<Term Prime> -> <Empty>")


# R27
def Factor():
  if currentLexeme == '-':
    masterList.append("<Factor> -> - <Primary>")
    lexer()
    Primary()
  else:
    masterList.append("<Factor> -> <Primary>")
    Primary()


# R28
def Primary():
  if currentLexeme == "true":
    masterList.append("<Primary> -> true")
    lexer()
  elif currentLexeme == "false":
    masterList.append("<Primary> -> false")
    lexer()
  elif currentToken == "IDENTIFIER" and nextLexeme != "(":
    masterList.append("<Primary> -> <Identifier>")
    Identifier()
  elif currentToken == "INTEGER":
    masterList.append("<Primary> -> <Integer>")
    Integer()
  elif currentToken == "IDENTIFIER" and nextLexeme == "(":
    masterList.append("<Primary> -> <Identifier> ( <IDs> )")
    Identifier()
    if currentLexeme == "(":
      lexer()
    IDs()
    if currentLexeme == ")":
      lexer()
  elif currentLexeme == '(':
    masterList.append("<Primary> -> ( <Expression> )")
    if currentLexeme == "(":
      lexer()
    Expression()
    if currentLexeme == ")":
      lexer()
  elif currentToken == "REAL":
    masterList.append("<Primary> -> <Real>")
    Real()
  else:
    masterList.append("Syntax Error: a token is expected after <Primary>")
    syntaxError()


# R29
def Empty():
  print("")


def syntaxError():
  if syntaxAnalyzerSwitch == "y":
    with open(outputFileName + ".txt", 'w') as outputFile:
      for line in masterList:
        outputFile.write(str(line) + "\n")
  elif syntaxAnalyzerSwitch == "n":
    with open(outputFileName + ".txt", 'w') as outputFile:
      outputFile.write("{:<24} {}\n".format("-Token-", "-Lexeme-"))
      for line in tokenLexemeList:
        outputFile.write("{:<25} {}\n".format(line[0], line[1]))
  quit()


# Start of "main"

# Getting user input for these variables.
fileName = input(
  "Enter the name of the .txt file you want to READ (only the name, excluding the '.txt'): "
)
outputFileName = input(
  "Enter the name of the .txt file you want to OUTPUT (only the name, excluding the '.txt'): "
)
syntaxAnalyzerSwitch = input(
  "Do you want to turn on the Syntax Analyzer Switch? ('y' for yes, 'n' for no): "
)

if not (syntaxAnalyzerSwitch == "y" or syntaxAnalyzerSwitch == "n"):
  print("Invalid user input for switch (please enter either 'y' or 'n'")

# Opening and reading fileName
f = open(fileName + ".txt", "r")
file = f.read()

# Running the lexical analyzer
while (c < len(file)):
  LA(file)

currentToken = tokenLexemeList[0][0]
currentLexeme = tokenLexemeList[0][1]

# Running the syntax analyzer
Rat23S()

if syntaxAnalyzerSwitch == "y":
  # Writes the masterList container into a .txt file
  with open(outputFileName + ".txt", 'w') as outputFile:
    for line in masterList:
      outputFile.write(str(line) + "\n")
elif syntaxAnalyzerSwitch == "n":
  with open(outputFileName + ".txt", 'w') as outputFile:
    outputFile.write("{:<24} {}\n".format("-Token-", "-Lexeme-"))
    for line in tokenLexemeList:
      outputFile.write("{:<25} {}\n".format(line[0], line[1]))

# Writes the instr_table container into a .txt file called AssemblyCode
with open("AssemblyCode" + ".txt", 'w') as outputFile:
  outputFile.write("{:<10} {:<10} {}\n".format("-Address-", "-Operator-",
                                               "-Operand-"))
  for i in instr_table:
    outputFile.write("{:<11} {:<11} {}\n".format(i.address, i.operator,
                                                 i.operand))
  outputFile.write("\n")
  outputFile.write("{:<10} {:<10} {}\n".format("-Identifier-",
                                               "-MemoryLocation-", "-Type-"))
  for i in symbolTable:
    outputFile.write("{:<14} {:<14} {}\n".format(i[0], i[1], i[2]))

print(idTable)
for i in idTable:
  print(i[0])
