type op = Add | Sub | Mult | Div | Mod | Exp | Equal | Neq | Less | Leq | Greater | Geq

type expr =
    LiteralBool of bool
  | LiteralInt of int
  | LiteralFloat of float
  | LiteralChar of char
  | LiteralString of string
  | Id of string
  | Brick of string * int * int * int * int (* color, height, width, x, y *)
  | Player of string * string * int * int * int (* color, shape, height, width, y *)
  | Map of int * int * expr
  | Ref of expr * expr
  | Binop of expr * op * expr
  | Not of expr
  | Assign of string * expr
  | Call of string * expr list
  | Noexpr

type stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt

type func_decl = {
    fname : string;
    formals : string list;
    locals : string list;
    body : stmt list;
    rettype: string;
}

type var_decl = {
    vartype : string;
    varname : string;
    varsize: int;
}

type brick = {
  mutable color: string;
  mutable height: int;
  mutable width: int;
  mutable x: int;
  mutable y: int;
}

type player = {
  mutable color: string;
  mutable shape: string;
  mutable height: int;
  mutable width: int;
  mutable y: int;
}

type map = {
  mutable height: int;
  mutable width: int;
  mutable generator: string;
}

type program = string list * func_decl list

let getInt v = 
    match v with
      LiteralInt(v) -> v
    | _ -> 0
        
let getBool v = 
    match v with
      LiteralBool(v) -> v
    | LiteralInt(1) -> true    
    | _ -> false

let getFloat v = 
    match v with
      LiteralFloat(v) -> v
    | _ -> 0.0
        
let getChar v = 
    match v with
      LiteralChar(v) -> v    
    | _ -> ''

let getString v = 
    match v with
      LiteralString(v) -> v
    | _ -> ""

let getMap v = 
    match v with
      Map(v) -> v
    | _ -> {height: 0; width: 0; generator: nil}

let getPlayer v = 
    match v with
      Player(v) -> v
    | _ -> {color: nil; shape: nil; height: 0; width: 0; y: 0}

let rec string_of_expr = function
    LiteralInt(l) -> string_of_int l
  | LiteralBool(l) -> string_of_bool l
  | LiteralFloat(l) -> string_of_float l
  | LiteralChar(l) -> string_of_char l
  | LiteralString(l) -> l
  | Id(s) -> s
  | Brick(s, i1, i2, i3, i4) -> 
    "Brick {\ncolor: " ^ s ^ 
      ",\nheight: " ^ i1 ^ 
      ",\nwidth: " ^ i2 ^
      ",\nx: " ^ i3 ^ 
      ",\ny: " ^ i4 ^ "}"
  | Player(s1, s2, i1, i2, i3) ->
    "Player {\ncolor: " ^ s1 ^
      ",\nshape: " ^ s2 ^
      ",\nheight: " ^ i1 ^
      ",\nwidth: " ^ i2 ^
      ",\ny: " ^ i3 ^ "}"
  | Map(i1, i2, e) ->
    "Map {\nheight: " ^ i1 ^
      ",\nwidth: " ^ i2 ^
      ",\ngenerator: " ^ string_of_expr e
  | Ref(s1, s2) ->
    string_of_expr s1 ^ "." ^ string_of_expr s2
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^
      (match o with
	Add -> "+" | Sub -> "-" | Mult -> "*" | Div -> "/"
      | Equal -> "==" | Neq -> "!="
      | Less -> "<" | Leq -> "<=" | Greater -> ">" | Geq -> ">=") ^ " " ^
      string_of_expr e2
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Noexpr -> ""

let rec string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | For(e1, e2, e3, s) ->
      "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
      string_of_expr e3  ^ ") " ^ string_of_stmt s
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s

let string_of_vdecl id = "int " ^ id ^ ";\n"

let string_of_fdecl fdecl =
  fdecl.fname ^ "(" ^ String.concat ", " fdecl.formals ^ ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.locals) ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_program (vars, funcs) =
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs)
