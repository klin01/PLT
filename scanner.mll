{ open Parser }

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "/*"     { comment lexbuf }           (* Comments *)
| '('      { LPAREN }
| ')'      { RPAREN }
| '{'      { LBRACE }
| '}'      { RBRACE }
| ';'      { SEMI }
| ','      { COMMA }
| '+'      { PLUS }
| '-'      { MINUS }
| '*'      { TIMES }
| '/'      { DIVIDE }
| '='      { ASSIGN }
| "=="     { EQ }
| "!="     { NEQ }
| '<'      { LT }
| "<="     { LEQ }
| ">"      { GT }
| ">="     { GEQ }
| "if"     { IF }
| "else"   { ELSE }
| "for"    { FOR }
| "while"  { WHILE }
| "return" { RETURN }
| "int"    { INT }
| ['0'-'9']+ as lxm { LITERAL(int_of_string lxm) }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }


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
| "!=" { NEQ } | '<' { LT }
| "<=" { LEQ } | '>' { GT }
| ">=" { GEQ } | "if" { IF } (* keywords *)
| "&&" { AND } | "||" { OR } | '!' { NOT }
| "else" { ELSE } | "for" { FOR }
| "while" { WHILE } | "return" { RETURN }

| "void" { TYPE("void") }
| "int" { TYPE("int") } | "float" { TYPE("float") } 
| "char" { TYPE("char") } | "string" { TYPE("string") } 
| "bool" { TYPE("bool") }
| "Array" { "Array" } (* | "Array" { TYPE("Array") } *) (* | "Image" { IMG } *)
| "Map" { TYPE("Map") }
| "function" { FUNC }
(*)
| "PlayerObj" { PLAYEROBJ }
| "Object" { OBJ } | "EnvObj" { ENVOBJ }
| "ActObj" { ACTOBJ } | "EventManager" { EVENTMGR } 
*)
| "true" { LITERALBOOL((*bool_of_string*) true) } | "false" { LITERALBOOL((*bool_of_string*) false) }
| eof { EOF } (* End-of-file *)
| ['0'-'9']+ as lxm { LITERALINT(int_of_string lxm) } (* integers *)
| ['0'-'9']*'.'['0'-'9']+ as lxm { LITERALFLOAT(float_of_string lxm) } (* floats *)
| ("'\\''" | '\''[^'\'''\t''\r''\n']'\'') as chr { LITERALCHAR(String.sub chr 1 ((String.length chr) - 2 )) }
| '"'([^'"'] | '\\''"')*'"' as str { LITERALSTRING(String.sub str 1 ((String.length str) - 2 )) }
| '$'['a'-'z' 'A'-'Z']+['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and multicomment = parse
"*/" { token lexbuf } (* End-of-comment *)
| eof { raise ( Failure("eof reached before multicomment completion")) }
| _ { multicomment lexbuf } (* Eat everything else *)

and singlecomment = parse
'\n' { token lexbuf } (* End-of-comment *)
| _ { singlecomment lexbuf } (* Eat everything else *)