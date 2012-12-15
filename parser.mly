%{ open Ast %}


%token SEMI LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK COMMA
%token PLUS MINUS TIMES DIVIDE ASSIGN
%token EQ NEQ LT LEQ GT GEQ
%token RETURN IF ELSE FOR WHILE INT

%token SHORTADD SHORTMINUS SHORTTIMES SHORTDIVIDE MOD EXP REF
%token FUNC
%token AND OR NOT
%token <string> TYPE
%token ARRAY /*IMG PLAYEROBJ OBJ ENVOBJ ACTOBJ EVENTMGR*/
%token <string> QUOTE

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
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE

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
fdecl:
   FUNC ID ASSIGN TYPE LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
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
    formal_decl { $1 }
  | formal_list COMMA formal_decl { $3 :: $1}

formal_decl:
    TYPE ID { { vartype: $1; varname: $2; varsize: 1 } }
  | ARRAY TYPE ID { { vartype: "Array" ^ $2; varname: $3; varsize: 0 } }

vdecl_list:
    /* nothing */    { [] }
  | vdecl_list vdecl { $2 :: $1 }

/* TODO: How to declare and assign variables in one statement? 
  Something like 
  int $myInt = 5; */
vdecl:
    TYPE ID SEMI { { vartype : $1; varname : $2; varsize: 1 } } 
  | ARRAY TYPE ID SEMI { { vartype: "Array" ^ $2 ; varname $3; varsize: 0 } }

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
  | ID               { Id($1) }
  | ID REF ID { Ref(Id($1), Id($3)) }
  | BRICK LBRACE COLOR ASSIGN LITERALSTRING COMMA \
    HEIGHT ASSIGN LITERALINT COMMA \
    WIDTH ASSIGN LITERALINT COMMA \
    XCOORD ASSIGN LITERALINT COMMA \
    YCOORD ASSIGN LITERALINT RBRACE { Brick($5, $9, $13, $17, $21) }
  | PLAYER LBRACE COLOR ASSIGN LITERALSTRING COMMA \
    SHAPE ASSIGN LITERALSTRING COMMA \
    HEIGHT ASSIGN LITERALINT COMMA \
    WIDTH ASSIGN LITERALINT COMMA \
    YCOORD ASSIGN LITERALINT RBRACE { Player($5, $9, $13, $17, $21) }
  | MAP LBRACE HEIGHT ASSIGN LITERALINT COMMA \
    WIDTH ASSIGN LITERALINT COMMA \
    GENERATOR ASSIGN ID RBRACE { Map($5, $9, $13) }
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
  | expr AND    expr { Binop($1, And,   $3) }
  | expr SHORTADD expr { Assign($1, Binop($1, Add, $3)) }
  | expr SHORTMINUS expr { Assign($1, Binop($1, Sub, $3)) }
  | expr SHORTTIMES expr { Assign($1, Binop($1, Mult, $3)) }
  | expr SHORTDIVIDE expr { Assign($1, Binop($1, Div, $3)) }
  | NOT  expr { Not($2) }
  | ID ASSIGN expr   { Assign($1, $3) }
  | ID LPAREN actuals_opt RPAREN { Call($1, $3) }
  | LPAREN expr RPAREN { $2 }

actuals_opt:
    /* nothing */ { [] }
  | actuals_list  { List.rev $1 }

actuals_list:
    expr                    { [$1] }
  | actuals_list COMMA expr { $3 :: $1 }
