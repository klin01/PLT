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
  | RUN
  | FUNC
  | ARRAY
  | BRICK
  | MAP
  | PLAYER
  | HEIGHT
  | WIDTH
  | SHAPE
  | COLOR
  | XCOORD
  | YCOORD
  | GENERATOR
  | TYPE of (string)
  | LITERALBOOL of (bool)
  | LITERALINT of (int)
  | LITERALFLOAT of (float)
  | LITERALCHAR of (char)
  | LITERALSTRING of (string)
  | ID of (string)
  | EOF

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
