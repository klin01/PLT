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
  | SHORTADD
  | SHORTMINUS
  | SHORTTIMES
  | SHORTDIVIDE
  | MOD
  | EXP
  | REF
  | INVOKE
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
  | INT
  | AND
  | OR
  | NOT
  | NEW
  | FUNC
  | ARRAY
  | BRICK
  | MAP
  | PLAYER
  | TYPE of (string)
  | LITERALINT of (int)
  | LITERALSTRING of (string)
  | ID of (string)
  | EOF

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
