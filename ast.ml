type op = Add | Sub | Mult | Div | Mod | Exp | Equal | Neq | Less | Leq | Greater | Geq | And | Or

(* TODO: Compiler will complain if we don't use "string" in Map and Brick *)

type expr =
    LiteralInt of int                  (* Integers *)
  | LiteralString of string            (* Strings *)
  | Id of string                       (* reference a variable *)
  | Brick of expr * string * expr * expr (* construct a Brick: Brick(color, array of points, x, y) *)
  | Player of expr * string * expr       (* construct Player: Player(color, array of points, y) *)
  | Array of string
  | Map of expr * expr * string          (* construct Map: Map(height, width, generator function) *)
  | Ref of expr * expr                 (* reference properties of object: Ref(parent, child) *)
  | AAccess of string * expr             (* array access: AAccess(arrayname, index) *)
  | AAssign of string * expr * expr      (* assign value to index of array: AAssign(arrayid, index, value) *)
  | Binop of expr * op * expr          (* binary operations: Binop(value, operator, value) *)
  | Not of expr                        (* boolean negation *)
  | Assign of string * expr              (* assign value to variable *)
  | Call of string * expr list           (* Call functions *)
  | Noexpr

type stmt =
    Block of stmt list                 (* block of statements *)
  | Expr of expr                       (* expressions *)
  | Return of expr                     (* return expression *)
  | If of expr * stmt * stmt           (* if statements *)
  | For of expr * expr * expr * stmt   (* for loops *)
  | While of expr * stmt               (* while loops *)

type var_decl = {
    vartype : string;                  (* variable type *)
    varname : string;                  (* variable name *)
}

type func_decl = {
    fname : string;                    (* function name *)
    formals : var_decl list;           (* function parameters *)
    locals : var_decl list;            (* function local variables *)
    body : stmt list;                  (* function body statements *)
    rettype : string;                  (* return type *)
}

type program = var_decl list * func_decl list

