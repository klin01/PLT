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

open Parsing;;
# 1 "parser.mly"
 open Ast 
# 61 "parser.ml"
let yytransl_const = [|
  257 (* SEMI *);
  258 (* LPAREN *);
  259 (* RPAREN *);
  260 (* LBRACE *);
  261 (* RBRACE *);
  262 (* LBRACK *);
  263 (* RBRACK *);
  264 (* COMMA *);
  265 (* PLUS *);
  266 (* MINUS *);
  267 (* TIMES *);
  268 (* DIVIDE *);
  269 (* ASSIGN *);
  270 (* EQ *);
  271 (* NEQ *);
  272 (* LT *);
  273 (* LEQ *);
  274 (* GT *);
  275 (* GEQ *);
  276 (* RETURN *);
  277 (* IF *);
  278 (* ELSE *);
  279 (* FOR *);
  280 (* WHILE *);
    0 (* EOF *);
  281 (* SHORTADD *);
  282 (* SHORTMINUS *);
  283 (* SHORTTIMES *);
  284 (* SHORTDIVIDE *);
  285 (* MOD *);
  286 (* EXP *);
  287 (* REF *);
  288 (* INT *);
  289 (* FLOAT *);
  290 (* CHAR *);
  291 (* STRING *);
  292 (* BOOL *);
  293 (* FUNC *);
  294 (* ARRAY *);
  295 (* IMG *);
  296 (* MAP *);
  297 (* PLAYEROBJ *);
  298 (* OBJ *);
  299 (* ENVOBJ *);
  300 (* ACTOBJ *);
  301 (* EVENTMGR *);
  302 (* AND *);
  303 (* OR *);
  304 (* NOT *);
  305 (* TRUE *);
  306 (* FALSE *);
    0|]

let yytransl_block = [|
  307 (* QUOTE *);
  308 (* LITERAL *);
  309 (* ID *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\001\000\003\000\004\000\004\000\007\000\007\000\
\005\000\005\000\002\000\006\000\006\000\008\000\008\000\008\000\
\008\000\008\000\008\000\008\000\009\000\009\000\009\000\009\000\
\009\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
\009\000\009\000\009\000\010\000\010\000\011\000\011\000\012\000\
\012\000\000\000"

let yylen = "\002\000\
\000\000\002\000\002\000\008\000\000\000\001\000\001\000\003\000\
\000\000\002\000\003\000\000\000\002\000\002\000\003\000\003\000\
\005\000\007\000\009\000\005\000\001\000\001\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\004\000\003\000\000\000\001\000\000\000\001\000\001\000\
\003\000\002\000"

let yydefred = "\000\000\
\001\000\000\000\000\000\000\000\000\000\002\000\003\000\000\000\
\000\000\011\000\007\000\000\000\000\000\000\000\000\000\009\000\
\008\000\000\000\010\000\000\000\000\000\012\000\004\000\000\000\
\000\000\000\000\000\000\021\000\000\000\013\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\014\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\035\000\016\000\015\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\025\000\
\026\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\034\000\000\000\000\000\000\000\020\000\000\000\
\000\000\000\000\018\000\000\000\000\000\019\000"

let yydgoto = "\002\000\
\003\000\006\000\007\000\012\000\018\000\020\000\013\000\030\000\
\031\000\056\000\059\000\060\000"

let yysindex = "\003\000\
\000\000\000\000\233\254\210\254\035\255\000\000\000\000\032\255\
\242\254\000\000\000\000\045\255\037\255\054\255\252\254\000\000\
\000\000\044\255\000\000\030\255\017\255\000\000\000\000\017\255\
\065\255\066\255\077\255\000\000\255\254\000\000\129\255\016\000\
\036\255\141\255\017\255\017\255\017\255\017\255\017\255\000\000\
\017\255\017\255\017\255\017\255\017\255\017\255\017\255\017\255\
\017\255\017\255\000\000\000\000\000\000\033\000\081\000\063\255\
\050\000\081\000\084\255\083\255\081\000\031\255\031\255\000\000\
\000\000\092\000\092\000\062\255\062\255\062\255\062\255\082\255\
\017\255\082\255\000\000\017\255\068\255\091\255\000\000\081\000\
\082\255\017\255\000\000\090\255\082\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\085\000\000\000\000\000\000\000\000\000\000\000\
\095\255\000\000\000\000\000\000\098\255\000\000\000\000\000\000\
\000\000\042\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\106\255\000\000\000\000\000\000\
\000\000\000\000\000\000\103\255\000\000\105\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\074\255\000\000\
\000\000\002\255\000\000\107\255\005\255\160\255\179\255\000\000\
\000\000\069\000\072\000\198\255\217\255\236\255\255\255\000\000\
\103\255\000\000\000\000\000\000\076\255\000\000\000\000\028\255\
\000\000\108\255\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\094\000\000\000\000\000\000\000\091\000\000\000\004\001\
\235\255\185\255\000\000\000\000"

let yytablesize = 367
let yytable = "\032\000\
\038\000\078\000\034\000\001\000\040\000\033\000\008\000\033\000\
\004\000\040\000\084\000\039\000\033\000\054\000\055\000\057\000\
\058\000\061\000\021\000\062\000\063\000\064\000\065\000\066\000\
\067\000\068\000\069\000\070\000\071\000\005\000\041\000\021\000\
\010\000\022\000\023\000\041\000\009\000\021\000\011\000\022\000\
\052\000\043\000\044\000\012\000\015\000\012\000\012\000\014\000\
\017\000\024\000\025\000\055\000\026\000\027\000\080\000\024\000\
\025\000\016\000\026\000\027\000\055\000\012\000\012\000\073\000\
\012\000\012\000\035\000\036\000\028\000\029\000\041\000\042\000\
\043\000\044\000\037\000\004\000\037\000\017\000\037\000\017\000\
\017\000\028\000\029\000\021\000\042\000\022\000\075\000\028\000\
\029\000\081\000\076\000\082\000\085\000\012\000\012\000\017\000\
\017\000\005\000\017\000\017\000\006\000\024\000\025\000\036\000\
\026\000\027\000\022\000\038\000\022\000\039\000\036\000\019\000\
\033\000\022\000\022\000\022\000\022\000\022\000\000\000\022\000\
\022\000\022\000\022\000\022\000\022\000\000\000\000\000\017\000\
\017\000\040\000\000\000\000\000\000\000\028\000\029\000\000\000\
\000\000\041\000\042\000\043\000\044\000\053\000\045\000\046\000\
\047\000\048\000\049\000\050\000\000\000\041\000\042\000\043\000\
\044\000\000\000\045\000\046\000\047\000\048\000\049\000\050\000\
\023\000\000\000\023\000\000\000\000\000\000\000\000\000\023\000\
\023\000\023\000\000\000\000\000\000\000\023\000\023\000\023\000\
\023\000\023\000\023\000\024\000\000\000\024\000\000\000\000\000\
\000\000\000\000\024\000\024\000\024\000\000\000\000\000\000\000\
\024\000\024\000\024\000\024\000\024\000\024\000\029\000\000\000\
\029\000\000\000\000\000\000\000\000\000\029\000\000\000\000\000\
\000\000\000\000\000\000\029\000\029\000\029\000\029\000\029\000\
\029\000\030\000\000\000\030\000\000\000\000\000\000\000\000\000\
\030\000\000\000\000\000\000\000\000\000\000\000\030\000\030\000\
\030\000\030\000\030\000\030\000\031\000\000\000\031\000\000\000\
\000\000\000\000\000\000\031\000\000\000\000\000\000\000\000\000\
\000\000\031\000\031\000\031\000\031\000\031\000\031\000\032\000\
\000\000\032\000\000\000\000\000\000\000\000\000\032\000\000\000\
\000\000\000\000\000\000\000\000\032\000\032\000\032\000\032\000\
\032\000\032\000\051\000\000\000\000\000\000\000\000\000\000\000\
\041\000\042\000\043\000\044\000\000\000\045\000\046\000\047\000\
\048\000\049\000\050\000\072\000\000\000\000\000\000\000\000\000\
\000\000\041\000\042\000\043\000\044\000\000\000\045\000\046\000\
\047\000\048\000\049\000\050\000\074\000\000\000\000\000\000\000\
\000\000\000\000\041\000\042\000\043\000\044\000\000\000\045\000\
\046\000\047\000\048\000\049\000\050\000\027\000\000\000\027\000\
\028\000\000\000\028\000\077\000\027\000\079\000\000\000\028\000\
\000\000\000\000\027\000\027\000\083\000\028\000\028\000\000\000\
\086\000\041\000\042\000\043\000\044\000\000\000\045\000\046\000\
\047\000\048\000\049\000\050\000\041\000\042\000\043\000\044\000\
\000\000\000\000\000\000\047\000\048\000\049\000\050\000"

let yycheck = "\021\000\
\002\001\073\000\024\000\001\000\003\001\001\001\053\001\003\001\
\032\001\008\001\082\000\013\001\008\001\035\000\036\000\037\000\
\038\000\039\000\002\001\041\000\042\000\043\000\044\000\045\000\
\046\000\047\000\048\000\049\000\050\000\053\001\003\001\002\001\
\001\001\004\001\005\001\008\001\002\001\002\001\053\001\004\001\
\005\001\011\001\012\001\002\001\008\001\004\001\005\001\003\001\
\053\001\020\001\021\001\073\000\023\001\024\001\076\000\020\001\
\021\001\004\001\023\001\024\001\082\000\020\001\021\001\001\001\
\023\001\024\001\002\001\002\001\052\001\053\001\009\001\010\001\
\011\001\012\001\001\001\032\001\003\001\002\001\002\001\004\001\
\005\001\052\001\053\001\002\001\000\000\004\001\003\001\052\001\
\053\001\022\001\008\001\001\001\003\001\052\001\053\001\020\001\
\021\001\003\001\023\001\024\001\003\001\020\001\021\001\001\001\
\023\001\024\001\001\001\003\001\003\001\003\001\003\001\018\000\
\022\000\008\001\009\001\010\001\011\001\012\001\255\255\014\001\
\015\001\016\001\017\001\018\001\019\001\255\255\255\255\052\001\
\053\001\001\001\255\255\255\255\255\255\052\001\053\001\255\255\
\255\255\009\001\010\001\011\001\012\001\001\001\014\001\015\001\
\016\001\017\001\018\001\019\001\255\255\009\001\010\001\011\001\
\012\001\255\255\014\001\015\001\016\001\017\001\018\001\019\001\
\001\001\255\255\003\001\255\255\255\255\255\255\255\255\008\001\
\009\001\010\001\255\255\255\255\255\255\014\001\015\001\016\001\
\017\001\018\001\019\001\001\001\255\255\003\001\255\255\255\255\
\255\255\255\255\008\001\009\001\010\001\255\255\255\255\255\255\
\014\001\015\001\016\001\017\001\018\001\019\001\001\001\255\255\
\003\001\255\255\255\255\255\255\255\255\008\001\255\255\255\255\
\255\255\255\255\255\255\014\001\015\001\016\001\017\001\018\001\
\019\001\001\001\255\255\003\001\255\255\255\255\255\255\255\255\
\008\001\255\255\255\255\255\255\255\255\255\255\014\001\015\001\
\016\001\017\001\018\001\019\001\001\001\255\255\003\001\255\255\
\255\255\255\255\255\255\008\001\255\255\255\255\255\255\255\255\
\255\255\014\001\015\001\016\001\017\001\018\001\019\001\001\001\
\255\255\003\001\255\255\255\255\255\255\255\255\008\001\255\255\
\255\255\255\255\255\255\255\255\014\001\015\001\016\001\017\001\
\018\001\019\001\003\001\255\255\255\255\255\255\255\255\255\255\
\009\001\010\001\011\001\012\001\255\255\014\001\015\001\016\001\
\017\001\018\001\019\001\003\001\255\255\255\255\255\255\255\255\
\255\255\009\001\010\001\011\001\012\001\255\255\014\001\015\001\
\016\001\017\001\018\001\019\001\003\001\255\255\255\255\255\255\
\255\255\255\255\009\001\010\001\011\001\012\001\255\255\014\001\
\015\001\016\001\017\001\018\001\019\001\001\001\255\255\003\001\
\001\001\255\255\003\001\072\000\008\001\074\000\255\255\008\001\
\255\255\255\255\014\001\015\001\081\000\014\001\015\001\255\255\
\085\000\009\001\010\001\011\001\012\001\255\255\014\001\015\001\
\016\001\017\001\018\001\019\001\009\001\010\001\011\001\012\001\
\255\255\255\255\255\255\016\001\017\001\018\001\019\001"

let yynames_const = "\
  SEMI\000\
  LPAREN\000\
  RPAREN\000\
  LBRACE\000\
  RBRACE\000\
  LBRACK\000\
  RBRACK\000\
  COMMA\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIVIDE\000\
  ASSIGN\000\
  EQ\000\
  NEQ\000\
  LT\000\
  LEQ\000\
  GT\000\
  GEQ\000\
  RETURN\000\
  IF\000\
  ELSE\000\
  FOR\000\
  WHILE\000\
  EOF\000\
  SHORTADD\000\
  SHORTMINUS\000\
  SHORTTIMES\000\
  SHORTDIVIDE\000\
  MOD\000\
  EXP\000\
  REF\000\
  INT\000\
  FLOAT\000\
  CHAR\000\
  STRING\000\
  BOOL\000\
  FUNC\000\
  ARRAY\000\
  IMG\000\
  MAP\000\
  PLAYEROBJ\000\
  OBJ\000\
  ENVOBJ\000\
  ACTOBJ\000\
  EVENTMGR\000\
  AND\000\
  OR\000\
  NOT\000\
  TRUE\000\
  FALSE\000\
  "

let yynames_block = "\
  QUOTE\000\
  LITERAL\000\
  ID\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    Obj.repr(
# 27 "parser.mly"
              ( [], [] )
# 348 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.program) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 28 "parser.mly"
                ( (_2 :: fst _1), snd _1 )
# 356 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.program) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'fdecl) in
    Obj.repr(
# 29 "parser.mly"
                ( fst _1, (_2 :: snd _1) )
# 364 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 5 : 'formals_opt) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_list) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 33 "parser.mly"
( { fname = _1;
formals = _3;
locals = List.rev _6;
body = List.rev _7 } )
# 377 "parser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 39 "parser.mly"
              ( [] )
# 383 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formal_list) in
    Obj.repr(
# 40 "parser.mly"
              ( List.rev _1 )
# 390 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 43 "parser.mly"
   ( [_1] )
# 397 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'formal_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 44 "parser.mly"
                       ( _3 :: _1 )
# 405 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 47 "parser.mly"
              ( [] )
# 411 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'vdecl_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 48 "parser.mly"
                   ( _2 :: _1 )
# 419 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 51 "parser.mly"
            ( _2 )
# 426 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 54 "parser.mly"
              ( [] )
# 432 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 55 "parser.mly"
                 ( _2 :: _1 )
# 440 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 58 "parser.mly"
          ( Expr(_1) )
# 447 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 59 "parser.mly"
                   ( Return(_2) )
# 454 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 60 "parser.mly"
                          ( Block(List.rev _2) )
# 461 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 61 "parser.mly"
                                          ( If(_3, _5, Block([])) )
# 469 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 62 "parser.mly"
                                       ( If(_3, _5, _7) )
# 478 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 6 : 'expr_opt) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : 'expr_opt) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'expr_opt) in
    let _9 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 64 "parser.mly"
( For(_3, _5, _7, _9) )
# 488 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 65 "parser.mly"
                                ( While(_3, _5) )
# 496 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 68 "parser.mly"
        ( Literal(_1) )
# 503 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 69 "parser.mly"
     ( Id(_1) )
# 510 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 70 "parser.mly"
                 ( Binop(_1, Add, _3) )
# 518 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 71 "parser.mly"
                  ( Binop(_1, Sub, _3) )
# 526 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 72 "parser.mly"
                  ( Binop(_1, Mult, _3) )
# 534 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 73 "parser.mly"
                   ( Binop(_1, Div, _3) )
# 542 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 74 "parser.mly"
               ( Binop(_1, Equal, _3) )
# 550 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 75 "parser.mly"
                ( Binop(_1, Neq, _3) )
# 558 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 76 "parser.mly"
               ( Binop(_1, Less, _3) )
# 566 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 77 "parser.mly"
                ( Binop(_1, Leq, _3) )
# 574 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 78 "parser.mly"
               ( Binop(_1, Greater, _3) )
# 582 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 79 "parser.mly"
                ( Binop(_1, Geq, _3) )
# 590 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 80 "parser.mly"
                 ( Assign(_1, _3) )
# 598 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'actuals_opt) in
    Obj.repr(
# 81 "parser.mly"
                               ( Call(_1, _3) )
# 606 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 82 "parser.mly"
                     ( _2 )
# 613 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 85 "parser.mly"
              ( Noexpr )
# 619 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 86 "parser.mly"
       ( _1 )
# 626 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    Obj.repr(
# 89 "parser.mly"
              ( [] )
# 632 "parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'actuals_list) in
    Obj.repr(
# 90 "parser.mly"
               ( List.rev _1 )
# 639 "parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 92 "parser.mly"
     ( [_1] )
# 646 "parser.ml"
               : 'actuals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'actuals_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 93 "parser.mly"
                          ( _3 :: _1 )
# 654 "parser.ml"
               : 'actuals_list))
(* Entry program *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.program)
