type token =
  | SEMI
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | LBRACK
  | RBRACK
  | COMMA
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | ASSIGN
  | EQ
  | NEQ
  | LT
  | LEQ
  | GT
  | GEQ
  | RETURN
  | IF
  | ELSE
  | FOR
  | WHILE
  | EOF
  | SHORTADD
  | SHORTMINUS
  | SHORTTIMES
  | SHORTDIVIDE
  | MOD
  | EXP
  | REF
  | INT
  | FLOAT
  | CHAR
  | STRING
  | BOOL
  | FUNC
  | ARRAY
  | IMG
  | MAP
  | PLAYEROBJ
  | OBJ
  | ENVOBJ
  | ACTOBJ
  | EVENTMGR
  | AND
  | OR
  | NOT
  | TRUE
  | FALSE
  | QUOTE of (string)
  | LITERAL of (int)
  | ID of (string)

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
