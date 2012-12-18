{ open Parser }

rule token = parse
[' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "/*" { multicomment lexbuf } (* Double Comments *)
| "//" { singlecomment lexbuf } (* Single Comments *)
| '(' { LPAREN } | ')' { RPAREN } (* punctuation *)
| '{' { LBRACE } | '}' { RBRACE }
| '[' { LBRACK } | ']' { RBRACK }
| ';' { SEMI } | ',' { COMMA } | '.' { REF }
| "+:" { SHORTADD } | "-:" { SHORTMINUS }
| "*:" { SHORTTIMES } | "/:" { SHORTDIVIDE }
| '+' { PLUS } | '-' { MINUS }
| '*' { TIMES } | '/' { DIVIDE }
| ':' { ASSIGN } | '=' { EQ }
| '%' { MOD } | '^' { EXP }
| "!=" { NEQ } | '<' { LT }
| "<=" { LEQ } | '>' { GT }
| ">=" { GEQ } | "if" { IF } (* keywords *)
| "&&" { AND } | "||" { OR } | '!' { NOT }
| "else" { ELSE } | "for" { FOR }
| "while" { WHILE } | "return" { RETURN }
| "void" { TYPE("void") }
| "int" { TYPE("int") }
| "string" { TYPE("string") } 
| "Array" { ARRAY } 
| "Map" { MAP }
| "Player" { PLAYER }
| "Brick" { BRICK }
| "function" { FUNC }
| "true" { LITERALINT(1) } | "false" { LITERALINT(0) }
| ('-')?['0'-'9']+ as lxm { LITERALINT(int_of_string lxm) } (* +/- integers *)
(*| ['0'-'9']*'.'['0'-'9']+ as lxm { LITERALFLOAT(float_of_string lxm) } (* floats *)*)
(*| ("'\\''" | '\''[^'\'''\t''\r''\n']'\'') as chr { LITERALCHAR((String.sub chr 1 ((String.length chr) - 2 )).[0]) }*)
| '"'([^'"'] | '\\''"')*'"' as str { LITERALSTRING(String.sub str 1 ((String.length str) - 2 )) }
| "new" { NEW }
| '$'['a'-'z' 'A'-'Z']+['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
| eof { EOF } (* End-of-file *)
| _ as charac { raise (Failure("illegal character " ^ Char.escaped charac)) }

and multicomment = parse
"*/" { token lexbuf } (* End-of-comment *)
| eof { raise ( Failure("eof reached before multicomment completion")) }
| _ { multicomment lexbuf } (* Eat everything else *)

and singlecomment = parse
'\n' { token lexbuf } (* End-of-comment *)
| _ { singlecomment lexbuf } (* Eat everything else *)