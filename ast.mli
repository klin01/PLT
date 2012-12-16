type op = Add | Sub | Mult | Div | Mod | Exp | Equal | Neq | Less | Leq | Greater | Geq | And | Or

type expr =
    LiteralInt of int
  | LiteralString of string
  | Id of string
  | Array of string
  | Brick of expr * expr * expr * expr * expr (* color, height, width, x, y *)
  | Player of expr * expr * expr * expr * expr (* color, shape, height, width, y *)
  | Map of expr * expr * expr
  | Ref of expr * expr
<<<<<<< HEAD
  (*| CallRef of expr * expr * expr list*)
=======
>>>>>>> Added to compiler, still missing commands for Array
  | AAccess of expr * expr (* array access: arrayname, index*)
  | AAssign of expr * expr * expr
  | Binop of expr * op * expr
  | Not of expr
  | Assign of expr * expr
  | Call of expr * expr list
  | Noexpr

type stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt

type var_decl = {
    vartype : string;
    varname : string;
}

type func_decl = {
    fname : string;
    formals : var_decl list;
    locals : var_decl list;
    body : stmt list;
    rettype : string;
}

type brick = {
  mutable bcolor: expr;
  mutable bheight: expr;
  mutable bwidth: expr;
  mutable bx: expr;
  mutable by: expr;
}

type player = {
  mutable pcolor: expr;
  mutable pshape: expr;
  mutable pheight: expr;
  mutable pwidth: expr;
  mutable py: expr;
}

type map = {
  mutable mheight: expr;
  mutable mwidth: expr;
  mutable mgenerator: expr;
}

type program = var_decl list * func_decl list

