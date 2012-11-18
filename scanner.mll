{ open Parser }

rule token = parse
[’ ’ ’\t’ ’\n’] { token lexbuf } (* Whitespace *)
| "/*" { dcomment lexbuf } (* Double Comments *)
| "//" { scomment lexbuf } (* Single Comments *)
| ’(’ { LPAREN } | ’)’ { RPAREN } (* punctuation *)
| ’{’ { LBRACE } | ’}’ { RBRACE }
| '[' { LBRACK } | ']' { RBRACK }
| ’;’ { SEMI } | ’,’ { COMMA } | '.' { REF }
| ’+’ { PLUS } | ’-’ { MINUS }
| ’*’ { TIMES } | ’/’ { DIVIDE }
| ’:’ { ASSIGN } | '=' { EQ }
| "+:" { SHORTADD } | "-:" { SHORTMINUS }
| "*:" { SHORTTIMES } | "/:" { SHORTDIVIDE }
| '%' { MOD } | '^' { EXP }
| '\'' { singlequote lexbuf } | '\"' { dboulequote lexbuf }
| "!=" { NEQ } | ’<’ { LT }
| "<=" { LEQ } | ">" { GT }
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
| [’0’-’9’]+ as lxm { LITERAL(int_of_string lxm) } (* integers *)
| '$'['a'-'z' 'A'-'Z'][’a’-’z’ ’A’-’Z’ ’0’-’9’ ’_’]* as lxm { ID(lxm) }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and singlequote = parse
'\'' { token lexbuf } (* End-of-single quote *)

and doublequote = parse
'\"' { token lexbuf } (* End-of-double quote *)

and comment = parse
"*/" { token lexbuf } (* End-of-comment *)
| _ { comment lexbuf } (* Eat everything else *)
