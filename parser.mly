%{ open Ast %}
%token SEMI LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK COMMA
%token PLUS MINUS TIMES DIVIDE ASSIGN
%token SHORTADD SHORTMINUS SHORTTIMES SHORTDIVIDE MOD EXP REF INVOKE
%token EQ NEQ LT LEQ GT GEQ
%token RETURN IF ELSE FOR WHILE INT
%token AND OR NOT

%token NEW FUNC ARRAY BRICK MAP PLAYER
%token <string> TYPE
%token <int> LITERALINT
%token <string> LITERALSTRING
/* Should I define LITERAL for FLOAT, STRING, etc too? */
%token <string> ID
%token EOF

%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
%left SHORTADD SHORTMINUS SHORTTIMES SHORTDIVIDE
%left AND OR
%left NOT
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE MOD
%left EXP
%left REF INVOKE

%start program
%type <Ast.program> program

%%

program:
   /* nothing */ { [], [] }
 | program vdecl { ($2 :: fst $1), snd $1 }
 | program fdecl { fst $1, ($2 :: snd $1) }

/*
TODO: Allow vdecl_list to mix with body?
*/
types:
   TYPE         { $1 }
 | BRICK        { "Brick" }
 | PLAYER       { "Player" }
 | MAP          { "Map" }
 | ARRAY TYPE   { "Array" ^ $2 }
 | ARRAY BRICK  { "ArrayBrick" }
 | ARRAY PLAYER { "ArrayPlayer" }
 | ARRAY MAP    { "ArrayMap" }

fdecl:
   FUNC ID ASSIGN types LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
     { { fname = $2; 
         formals = $6; 
         locals = List.rev $9; 
         body = List.rev $10; 
         rettype = $4 } }

formals_opt:
    /* nothing */ { [] }
  | formal_list   { List.rev $1 }

formal_list:
    formal_decl                   { [$1] }
  | formal_list COMMA formal_decl { $3 :: $1 }

formal_decl:
    types ID         { { vartype= $1; varname= $2; } }

vdecl_list:
    /* nothing */    { [] }
  | vdecl_list vdecl { $2 :: $1 }

vdecl:
    types ID SEMI    { { vartype= $1; varname= $2; } } 

stmt_list:
    /* nothing */    { [] }
  | stmt_list stmt   { $2 :: $1 }

stmt:
    expr SEMI                     { Expr($1) }
  | RETURN expr SEMI              { Return($2) }
  | LBRACE stmt_list RBRACE       { Block(List.rev $2) }
  | IF LPAREN expr RPAREN stmt %prec NOELSE 
                                  { If($3, $5, Block([])) }
  | IF LPAREN expr RPAREN stmt ELSE stmt    
                                  { If($3, $5, $7) }
  | FOR LPAREN expr_opt SEMI expr_opt SEMI expr_opt RPAREN stmt
                                  { For($3, $5, $7, $9) }
  | WHILE LPAREN expr RPAREN stmt { While($3, $5) }

expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1 }

expr:
    LITERALINT            { LiteralInt($1) }
  | LITERALSTRING         { LiteralString($1) }
  | ID LBRACK expr RBRACK { AAccess(Id($1), $3) }
  | NEW BRICK LPAREN LITERALSTRING COMMA expr COMMA expr COMMA expr RPAREN  
                          { Brick(LiteralString($4), $6, $8, $10) }
  | NEW MAP LPAREN expr COMMA expr COMMA ID RPAREN 
                          { Map($4, $6, Id($8)) }
  | NEW PLAYER LPAREN LITERALSTRING COMMA expr COMMA expr RPAREN 
                          { Player(LiteralString($4), $6, $8) }
  | NEW ARRAY TYPE        { Array($3) }
  | NEW ARRAY BRICK       { Array("Brick") }
  | NEW ARRAY PLAYER      { Array("Player") }
  | NEW ARRAY MAP         { Array("Map") }
  | ID REF ID             { Id($1 ^ "." ^ $3) } /* Ref(Id($1), Id($3)) } */
  | ID                    { Id($1) }
  | expr PLUS   expr      { Binop($1, Add,   $3) }
  | expr MINUS  expr      { Binop($1, Sub,   $3) }
  | expr TIMES  expr      { Binop($1, Mult,  $3) }
  | expr DIVIDE expr      { Binop($1, Div,   $3) }
  | expr MOD    expr      { Binop($1, Mod,   $3) }
  | expr EXP    expr      { Binop($1, Exp,   $3) }
  | expr EQ     expr      { Binop($1, Equal, $3) }
  | expr NEQ    expr      { Binop($1, Neq,   $3) }
  | expr LT     expr      { Binop($1, Less,  $3) }
  | expr LEQ    expr      { Binop($1, Leq,   $3) }
  | expr GT     expr      { Binop($1, Greater, $3) }
  | expr GEQ    expr      { Binop($1, Geq,     $3) }
  | expr SHORTADD expr    { Assign($1, Binop($1, Add,  $3)) }
  | expr SHORTMINUS expr  { Assign($1, Binop($1, Sub,  $3)) }
  | expr SHORTTIMES expr  { Assign($1, Binop($1, Mult, $3)) }
  | expr SHORTDIVIDE expr { Assign($1, Binop($1, Div,  $3)) }
  | expr AND expr         { Binop($1, And, $3) }
  | expr OR  expr         { Binop($1, Or,  $3) }
  | NOT LITERALINT                    { Not(LiteralInt($2)) }
  | ID LBRACK expr RBRACK ASSIGN expr { AAssign(Id($1), $3, $6) }
  | ID ASSIGN expr                    { Assign(Id($1), $3) }
  | ID REF ID ASSIGN expr             { Assign(Id($1 ^ "." ^ $3), $5) }
  | ID LPAREN actuals_opt RPAREN      { Call(Id($1), $3) }
  | LPAREN expr RPAREN                { $2 }

actuals_opt:
    /* nothing */ { [] }
  | actuals_list  { List.rev $1 }

actuals_list:
    expr                    { [$1] }
  | actuals_list COMMA expr { $3 :: $1 }
