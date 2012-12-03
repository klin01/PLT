{ open Parser }

let escape = '\\'['\'' '\' 'n' 't' 'r' '\\']
let character = [^'\\' '\"']
let char_not_squote = [^'\\' '\'']

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
| '\'' { singlequote lexbuf }
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
| "true" { LITERALBOOL(bool_of_string true) } | "false" { LITERALBOOL(bool_of_string false) }
| eof { EOF } (* End-of-file *)
| ['0'-'9']+ as lxm { LITERALINT(int_of_string lxm) } (* integers *)
| ['0'-'9']*'.'['0'-'9']+ as lxm { LITERALFLOAT(float_of_string lxm) } (* floats *)
| '$'['a'-'z' 'A'-'Z']+['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and multicomment = parse
"*/" { token lexbuf } (* End-of-comment *)
| eof { raise ( Failure("eof reached before multicomment completion")) }
| _ { multicomment lexbuf } (* Eat everything else *)

and singlecomment = parse
'\n' { token lexbuf } (* End-of-comment *)
| _ { singlecomment lexbuf } (* Eat everything else *)

and doublequote = parse
(escape | character)* as lxm { LITERALSTRING(lxm) }
| '\"' { token lexbuf }
| eof { raise (Failure("eof reached before string completion")) }

and singlequote = parse
(escape | char_not_squote) as lxm { LITERALCHAR(lxm) }
| '\'' { token lexbuf }
| eof { raise (Failure("eof reached before char completion")) }

