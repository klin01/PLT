%{ open Ast %}
%token SEMI LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK COMMA
%token PLUS MINUS TIMES DIVIDE ASSIGN
%token SHORTADD SHORTMINUS SHORTTIMES SHORTDIVIDE MOD REF
%token EQ NEQ LT LEQ GT GEQ
%token RETURN IF ELSE FOR WHILE INT
%token AND OR NOT
%token NEW FUNC ARRAY BRICK MAP PLAYER
%token <string> TYPE
%token <int> LITERALINT
%token <string> LITERALSTRING
%token <string> ID
%token EOF

/* Define associativity of tokens */
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
%left REF INVOKE

/* Enters at 'program' */
%start program
%type <Ast.program> program

%%

/* Program type defined in the Ast is of the form: 
   var_decl list * func_decl list 
   So if you see a vdecl, add to var_decl list
   If you see a fdecl, add to the func_decl list */
program:
   /* nothing */ { [], [] }
 | program vdecl { ($2 :: fst $1), snd $1 }
 | program fdecl { fst $1, ($2 :: snd $1) }

/* Type declaration must be made separately from initialization */
types:
   TYPE         { $1 }
 | BRICK        { "Brick" }
 | PLAYER       { "Player" }
 | MAP          { "Map" }
 | ARRAY TYPE   { "Array" ^ $2 }
 | ARRAY BRICK  { "ArrayBrick" }
 | ARRAY PLAYER { "ArrayPlayer" }
 | ARRAY MAP    { "ArrayMap" }

/* Handle functions */
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

/* Handle variable declarations */
vdecl_list:
    /* nothing */    { [] }
  | vdecl_list vdecl { $2 :: $1 }

vdecl:
    types ID SEMI    { { vartype= $1; varname= $2; } } 

/* Handle statements */
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

/* Handle expressions */
expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1 }

expr:
    LITERALINT            { LiteralInt($1) }
  | LITERALSTRING         { LiteralString($1) }
  | expr PLUS   expr      { Binop($1, Add,   $3) }
  | expr MINUS  expr      { Binop($1, Sub,   $3) }
  | expr TIMES  expr      { Binop($1, Mult,  $3) }
  | expr DIVIDE expr      { Binop($1, Div,   $3) }
  | expr MOD    expr      { Binop($1, Mod,   $3) }
  | expr EQ     expr      { Binop($1, Equal, $3) }
  | expr NEQ    expr      { Binop($1, Neq,   $3) }
  | expr LT     expr      { Binop($1, Less,  $3) }
  | expr LEQ    expr      { Binop($1, Leq,   $3) }
  | expr GT     expr      { Binop($1, Greater, $3) }
  | expr GEQ    expr      { Binop($1, Geq,     $3) }
  | ID SHORTADD expr      { Assign($1, Binop(Id($1), Add,  $3)) }
  | ID SHORTMINUS expr    { Assign($1, Binop(Id($1), Sub,  $3)) }
  | ID SHORTTIMES expr    { Assign($1, Binop(Id($1), Mult, $3)) }
  | ID SHORTDIVIDE expr   { Assign($1, Binop(Id($1), Div,  $3)) }
  | expr AND expr         { Binop($1, And, $3) }
  | expr OR  expr         { Binop($1, Or,  $3) }
  | NOT expr              { Not($2) }
  | NEW BRICK LPAREN expr COMMA expr COMMA expr COMMA expr COMMA expr COMMA expr RPAREN
                          /* r, g, b, varray, x, y */
                          { Brick($4, $6, $8, $10, $12, $14) }
  | NEW MAP LPAREN expr COMMA expr COMMA ID RPAREN 
                          /* width, height, brick generating function */
                          { Map($4, $6, $8) }
  | NEW PLAYER LPAREN expr COMMA expr COMMA expr COMMA expr COMMA expr RPAREN 
                          /* r, g, b, varray, y */
                          { Player($4, $6, $8, $10, $12) }
  | NEW ARRAY TYPE        { Array($3) }
  | NEW ARRAY BRICK       { Array("Brick") }
  | NEW ARRAY PLAYER      { Array("Player") }
  | NEW ARRAY MAP         { Array("Map") }
  | ID                                        { Id($1) }
  | ID REF ID                                 { Id($1 ^ "." ^ $3) }
  | ID ASSIGN expr                            { Assign($1, $3) }
  | ID REF ID ASSIGN expr                     { Assign(($1 ^ "." ^ $3), $5) }
  | ID LBRACK expr RBRACK                     { AAccess($1, $3) } /* Handle arrays */
  | ID LBRACK expr RBRACK ASSIGN expr         { AAssign($1, $3, $6) }
  | ID REF ID LBRACK expr RBRACK              { AAccess(($1 ^ "." ^ $3), $5) } 
  | ID REF ID LBRACK expr RBRACK ASSIGN expr  { AAssign(($1 ^ "." ^ $3), $5, $8) }
  | ID LPAREN actuals_opt RPAREN              { Call($1, $3) }
  | LPAREN expr RPAREN                        { $2 }

/* Handle actual values passed to functions */
actuals_opt:
    /* nothing */ { [] }
  | actuals_list  { List.rev $1 }

actuals_list:
    expr                    { [$1] }
  | actuals_list COMMA expr { $3 :: $1 }
