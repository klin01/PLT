type op = Add | Sub | Mult | Div | Mod | Equal | Neq | Less | Leq | Greater | Geq | And | Or

type expr =
    LiteralInt of int                  (* Integers *)
  | LiteralString of string            (* Strings *)
  | Id of string                       (* Reference a variable *)
  | Brick of expr * expr * expr * expr * expr * expr (* Construct Brick: Brick(r, g, b, array of points, x, y) *)
  | Player of expr * expr * expr * expr * expr       (* Construct Player: Player(r, g, b, array of points, y) *)
  | Array of string                    (* Locate the array and initialize it by inserting a variable type identifier
                                          e.g. 6 for int array, 7 for string array *)
  | Map of expr * expr * string        (* Construct Map: Map(height, width, generator function) *)
  | AAccess of string * expr           (* Array access: AAccess(arrayid, index) *)
  | AAssign of string * expr * expr    (* Assign value to index of array: AAssign(arrayid, index, value) *)
  | Binop of expr * op * expr          (* Binary operations: Binop(value, operator, value) *)
  | Not of expr                        (* Boolean negation *)
  | Assign of string * expr            (* Assign value to variable *)
  | Call of string * expr list         (* Call functions *)
  | Noexpr

type stmt =
    Block of stmt list                 (* Block of statements *)
  | Expr of expr                       (* Expressions *)
  | Return of expr                     (* Return expression *)
  | If of expr * stmt * stmt           (* If statements *)
  | For of expr * expr * expr * stmt   (* For loops *)
  | While of expr * stmt               (* While loops *)

type var_decl = {
    vartype : string;                  (* Variable type *)
    varname : string;                  (* Variable name *)
}

type func_decl = {
    fname : string;                    (* Function name *)
    formals : var_decl list;           (* Function parameters *)
    locals : var_decl list;            (* Function local variables *)
    body : stmt list;                  (* Function body statements *)
    rettype : string;                  (* Return type *)
}

type program = var_decl list * func_decl list

