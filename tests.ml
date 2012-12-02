open Ast

let string2expr str =
  let lexbuf = Lexing.from_string str in
  Parser.expr Scanner.token lexbuf

let test expr = 
  match expr with
    Id(c) -> print_endline c
  | Binop(e1, PLUS, e2) -> print_endline (e1 + e2)
  | Binop(e1, op, e2) -> "Error"