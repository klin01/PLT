{ open Parser }

rule token = parse
[' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "/*" { multicomment lexbuf } (* Double Comments *)
| "//" { singlecomment lexbuf } (* Single Comments *)
| '(' { LPAREN } | ')' { RPAREN } (* punctuation *)
| '{' { LBRACE } | '}' { RBRACE }
| '[' { LBRACK } | ']' { RBRACK }
| ';' { SEMI } | ',' { COMMA } | '.' { REF }
| '+' { PLUS } | '-' { MINUS }
| '*' { TIMES } | '/' { DIVIDE }
| ':' { ASSIGN } | '=' { EQ }
| "+:" { SHORTADD } | "-:" { SHORTMINUS }
| "*:" { SHORTTIMES } | "/:" { SHORTDIVIDE }
| '%' { MOD } | '^' { EXP }
| '\"' { doublequote lexbuf }
| "!=" { NEQ } | '<' { LT }
| "<=" { LEQ } | '>' { GT }
| ">=" { GEQ } | "if" { IF } (* keywords *)
| "&&" { AND } | "||" { OR } | '!' { NOT }
| "else" { ELSE } | "for" { FOR }
| "while" { WHILE } | "return" { RETURN }
| "int" { INT } | "float" { FLOAT } 
| "char" { CHAR } | "string" { STRING } 
| "bool" { BOOL } | "function" { FUNC }
| "Array" { ARRAY } | "Image" { IMG }
| "Map" { MAP } | "PlayerObj" { PLAYEROBJ }
| "Object" { OBJ } | "EnvObj" { ENVOBJ }
| "ActObj" { ACTOBJ } | "EventManager" { EVENTMGR } 
| "true" { TRUE } | "false" { FALSE }
| eof { EOF } (* End-of-file *)
| ['0'-'9']+ as lxm { LITERAL(int_of_string lxm) } (* integers *)
| '$'['a'-'z' 'A'-'Z']+['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and multicomment = parse
"*/" { token lexbuf } (* End-of-comment *)
| eof { raise (Failure("eof reached before multicomment completion") }
| _ { comment lexbuf } (* Eat everything else *)

and singlecomment = parse
'\n' { token lexbuf } (* End-of-comment *)
| _ { comment lexbuf } (* Eat everything else *)

and doublequote = parse
'\"' { token lexbuf }
| '\\' { escaped lexbuf } 
| eof { raise (Failure("eof reached before string completion") }
| _ { doublequote lexbuf }

and escaped = parse
['\'' '\"' 'n' 't' 'r' '\\'] { doublequote lexbuf }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }
