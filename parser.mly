%{ open Ast %}
%token SEMI LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK COMMA
%token PLUS MINUS TIMES DIVIDE ASSIGN
%token SHORTADD SHORTMINUS SHORTTIMES SHORTDIVIDE MOD EXP REF INVOKE
%token EQ NEQ LT LEQ GT GEQ
%token RETURN IF ELSE FOR WHILE INT
%token AND OR NOT

%token NEW FUNC ARRAY BRICK MAP PLAYER HEIGHT WIDTH SHAPE COLOR XCOORD YCOORD GENERATOR
%token <string> TYPE
%token <bool> LITERALBOOL
%token <int> LITERALINT
%token <float> LITERALFLOAT
%token <char> LITERALCHAR
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
   TYPE { $1 }
 | BRICK { "Brick" }
 | PLAYER { "Player" }
 | MAP { "Map" }
 | ARRAY TYPE { "Array" ^ $2 }
 | ARRAY BRICK { "ArrayBrick" }
 | ARRAY PLAYER { "ArrayPlayer" }
 | ARRAY MAP { "ArrayMap" }

fdecl:
   FUNC ID ASSIGN types LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
   /*ID LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE*/
     { { fname = $2;
	 formals = $6;
	 locals = List.rev $9;
	 body = List.rev $10;
   rettype = $4 } }

formals_opt:
    /* nothing */ { [] }
  | formal_list   { List.rev $1 }

formal_list:
    formal_decl { [$1] }
  | formal_list COMMA formal_decl { $3 :: $1 }

formal_decl:
    TYPE ID { { vartype= $1; varname= $2; varsize= 1 } }
  | BRICK ID { { vartype= "Brick"; varname= $2; varsize= 1 } }
  | PLAYER ID { { vartype= "Player"; varname= $2; varsize= 1 } }
  | MAP ID { { vartype= "Map"; varname= $2; varsize= 1 } }
  | ARRAY TYPE ID { { vartype= "Array" ^ $2; varname= $3; varsize= 0 } }
  | ARRAY BRICK ID { { vartype= "ArrayBrick"; varname= $3; varsize= 0 } }
  | ARRAY PLAYER ID { { vartype= "ArrayPLayer"; varname= $3; varsize= 0 } }
  | ARRAY MAP ID { { vartype= "ArrayMap"; varname= $3; varsize= 0 } }

vdecl_list:
    /* nothing */    { [] }
  | vdecl_list vdecl { $2 :: $1 }

/* TODO: How to declare and assign variables in one statement? 
  Something like 
  int $myInt = 5; */
vdecl:
    TYPE ID SEMI { { vartype= $1; varname= $2; varsize= 1 } } 
  | BRICK ID SEMI { { vartype= "Brick"; varname= $2; varsize= 1 } }
  | PLAYER ID SEMI { { vartype= "Player"; varname= $2; varsize= 1 } }
  | MAP ID SEMI { { vartype= "Map"; varname= $2; varsize= 1 } }
  | ARRAY TYPE ID SEMI { { vartype= "Array" ^ $2; varname= $3; varsize= 0 } }
  | ARRAY BRICK ID SEMI { { vartype= "ArrayBrick"; varname= $3; varsize= 0 } }
  | ARRAY PLAYER ID SEMI { { vartype= "ArrayPLayer"; varname= $3; varsize= 0 } }
  | ARRAY MAP ID SEMI { { vartype= "ArrayMap"; varname= $3; varsize= 0 } }

stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }

stmt:
    expr SEMI { Expr($1) }
  | RETURN expr SEMI { Return($2) }
  | LBRACE stmt_list RBRACE { Block(List.rev $2) }
  | IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7) }
  | FOR LPAREN expr_opt SEMI expr_opt SEMI expr_opt RPAREN stmt
     { For($3, $5, $7, $9) }
  | WHILE LPAREN expr RPAREN stmt { While($3, $5) }

expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1 }

expr:
    LITERALINT           { LiteralInt($1) }
  | LITERALFLOAT         { LiteralFloat($1) }
  | LITERALBOOL          { LiteralBool($1) }
  | LITERALCHAR          { LiteralChar($1) }
  | LITERALSTRING        { LiteralString($1) }
  | NEW ARRAY TYPE       { Array($3) }
  | NEW ARRAY BRICK      { Array("Brick") }
  | NEW ARRAY MAP        { Array("Map") }
  | NEW ARRAY PLAYER     { Array("Player") }
  | NEW BRICK LPAREN expr COMMA expr COMMA expr COMMA expr COMMA expr RPAREN { Brick($4, $6, $8, $10, $12) }
  | NEW MAP LPAREN expr COMMA expr COMMA expr RPAREN { Map($4, $6, $8) }
  | NEW PLAYER LPAREN expr COMMA expr COMMA expr COMMA expr COMMA expr RPAREN { Player($4, $6, $8, $10, $12) }
  | ID                   { Id($1) }
  | ID REF ID            { Ref(Id($1), Id($3)) }
  | ID INVOKE expr   { CallRef(Id($1), $3)}
  | expr PLUS   expr { Binop($1, Add,   $3) }
  | expr MINUS  expr { Binop($1, Sub,   $3) }
  | expr TIMES  expr { Binop($1, Mult,  $3) }
  | expr DIVIDE expr { Binop($1, Div,   $3) }
  | expr MOD    expr { Binop($1, Mod,   $3) }
  | expr EXP    expr { Binop($1, Exp,   $3) }
  | expr EQ     expr { Binop($1, Equal, $3) }
  | expr NEQ    expr { Binop($1, Neq,   $3) }
  | expr LT     expr { Binop($1, Less,  $3) }
  | expr LEQ    expr { Binop($1, Leq,   $3) }
  | expr GT     expr { Binop($1, Greater,  $3) }
  | expr GEQ    expr { Binop($1, Geq,   $3) }
  | expr AND    expr { Binop($1, And,   $3) }
  | expr OR     expr { Binop($1, Or,   $3) }
  | expr SHORTADD expr { Assign($1, Binop($1, Add, $3)) }
  | expr SHORTMINUS expr { Assign($1, Binop($1, Sub, $3)) }
  | expr SHORTTIMES expr { Assign($1, Binop($1, Mult, $3)) }
  | expr SHORTDIVIDE expr { Assign($1, Binop($1, Div, $3)) }
  | NOT  expr { Not($2) }
  | expr ASSIGN expr   { Assign($1, $3) }
  | ID LPAREN actuals_opt RPAREN { Call($1, $3) }
  | LPAREN expr RPAREN { $2 }

actuals_opt:
    /* nothing */ { [] }
  | actuals_list  { List.rev $1 }

actuals_list:
    expr                    { [$1] }
  | actuals_list COMMA expr { $3 :: $1 }
