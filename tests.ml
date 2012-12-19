open Ast

let string2expr str =
  let lexbuf = Lexing.from_string str in
  Parser.program Scanner.token lexbuf

(*let getType v = 
  match v with
      LiteralInt(v) -> "int"
    | LiteralBool(v) -> "bool"
    | LiteralString(v) -> "string"
    | LiteralFloat(v) -> "float"
    | LiteralChar(v) -> "char"
    | Map(v1, v2, v3) -> "Map"
    | Player(v1, v2, v3, v4, v5) -> "Player"
    | Brick(v1, v2, v3, v4, v5) -> "Brick"  *)

let getInt v = 
    match v with
      LiteralInt(v) -> v
    | _ -> 0
        
let getBool v = 
    match v with
      LiteralInt(1) -> true    
    | _ -> false

let getString v = 
    match v with
      LiteralString(v) -> v
    | _ -> ""

(*
let getMap v = 
    match v with
      Map(v1, v2, v3) -> { mheight= v1; mwidth= v2; mgenerator= v3 }
    | _ -> { mheight= LiteralInt(0); mwidth= LiteralInt(0); mgenerator= LiteralString("test") }

let getPlayer v = 
    match v with
      Player(c, a, x, y) -> { pcolor= v1; pshape= v2; pheight= v3; pwidth= v4; py= v5 }
    | _ -> { pcolor= LiteralString("red"); pshape= LiteralString("triangle"); pheight= LiteralInt(0); pwidth= LiteralInt(0); py= LiteralInt(0) }

let getBrick v = 
    match v with
      Brick(v1, v2, v3, v4, v5) -> { bcolor= v1; bheight= v2; bwidth= v3; bx= v4; by= v5 }
    | _ -> { bcolor= LiteralString("red"); bheight= LiteralInt(0); bwidth= LiteralInt(0); bx= LiteralInt(0); by= LiteralInt(0) }
*)
let rec string_of_expr = function
    LiteralInt(l) -> string_of_int l
  | LiteralString(l) -> "\"" ^ l ^ "\""
  | Id(s) -> s
  | Brick(r, g, b, a, x, y) -> 
    "Brick(" ^ string_of_expr r ^
      ", " ^ string_of_expr g ^
      ", " ^ string_of_expr b ^  
      ", " ^ string_of_expr a ^ 
      ", " ^ string_of_expr x ^
      ", " ^ string_of_expr y ^ ")"
  | Player(r, g, b, a, y) ->
    "Player(" ^  string_of_expr r ^
      ", " ^ string_of_expr g ^
      ", " ^ string_of_expr b ^
      ", " ^ string_of_expr a ^
      ", " ^ string_of_expr y ^ ")"
  | Map(i1, i2, e) ->
    "Map(" ^ string_of_expr i1 ^
      ", " ^ string_of_expr i2 ^
      ", " ^  e ^ ")"
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^
      (match o with
        Add -> "+" | Sub -> "-" | Mult -> "*" | Div -> "/"
      | Equal -> "=" | Neq -> "!:"
      | Less -> "<" | Leq -> "<:" | Greater -> ">" | Geq -> ">:" | And -> "&&"
      | Or -> "||" | Mod -> "%" ) ^ " " ^
      string_of_expr e2
  | Array(a) -> "new Array " ^ a 
  | Assign(v, e) ->  v ^ " : " ^ string_of_expr e
  | AAssign(a, i, e) ->  a ^ "[" ^ string_of_expr i ^ "] : " ^ string_of_expr e
  | AAccess(a, i) ->  a ^ "[" ^ string_of_expr i ^ "]"
  | Call(f, el) ->
       f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Not(e1) -> "!" ^ string_of_expr e1
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

let string_of_vdecl id = id.vartype ^ " " ^ id.varname ^ ";\n"



let string_of_fdecl fdecl =
  fdecl.fname ^ "(" ^ String.concat ", " (List.map string_of_vdecl fdecl.formals) ^ ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.locals) ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_program (vars, funcs) =
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs)

let test =
 	string_of_program (string2expr("
int $globalvar1;
int $globalvar2;

//$globalvar2 : 1;
//$globalvar1 : 2;

function $main : void () {
  
  Map $gameMap;
  Player $me;
  int $test;

  Array int $myIntArray;
  $myIntArray[1] : 10;
  $test.$x = 9;
  $test: -5;
  $gameMap: new Map(1024, 768, $generateThis);
  $me : new Player(\"12 12 12\", $testarray, 500);

  $Run($gameMap, $me);
}

function $generateThis: Array Brick () {
  //vdecls
  Array Brick $output;
  Brick $top;
  Brick $bottom;
  //stmts

  $top : new Brick(\"12 23 54\", $test2, 0, 1004);
  $Push($output, $top);
  $bottom : new Brick(\"12 2 34\", $test3, 0, 0);
  $Push($output, $bottom);
  return $output;
}"))