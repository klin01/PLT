open Ast

let string2expr str =
  let lexbuf = Lexing.from_string str in
  Parser.program Scanner.token lexbuf

let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Equal -> "="
  | Neq -> "!="
  | Less -> "<"
  | Leq -> "<="
  | Greater -> ">"
  | Geq -> ">="

let rec string_of_expr = function
    Literal(c) -> string_of_int c
  | Id(c) -> c
  | Binop(e1, op, e2) -> "(" ^ string_of_expr e1 ^ string_of_op op ^ string_of_expr e2 ^ ")"
  | Assign(e1, e2) -> e1 ^ "=" ^ string_of_expr e2
  | Call(e1, e2) -> e1 ^ "(" ^ List.fold_left (fun a b -> a ^ string_of_expr b) "" e2
  | Noexpr -> ""


let rec string_of_stmt = function
    Block(b) -> "{" ^ List.fold_left (fun a b -> a ^ string_of_stmt b) "" b ^ "}"
  | Expr(e1) -> "(" ^ string_of_expr e1 ^ ")"
  | Return(e1) -> "return " ^ string_of_expr e1 ^ ";"
  | If(e1, s1, s2) -> "If (" ^ string_of_expr e1 ^ "){" ^ string_of_stmt s1 ^ "} else {" ^ string_of_stmt s2 ^ "}"
  | For(e1, e2, e3, s1) -> "For(" ^ string_of_expr e1 ^ ";" ^ string_of_expr e2 ^ ";" ^ string_of_expr e3 ^ ") {" ^ string_of_stmt s1 ^ "}" 
  | While(e1, s1) -> "While(" ^ string_of_expr e1 ^ ") {" ^ string_of_stmt s1 ^ "}"

let string_of_funcdecl f =
  f.fname ^ "(" ^ (List.fold_left (fun a b -> a ^ "," ^ b) "" f.formals) ^ ") {" ^ (List.fold_left (fun a b -> a ^ "," ^ b) "" f.locals) ^ " " ^ List.fold_left (fun a b -> a ^ string_of_stmt b) "" f.body ^ "}"

let string_of_program p =
  List.fold_left(fun a b -> a ^ "," ^ b) "" (fst p) ^ "\n" ^ List.fold_left(fun a b -> a ^ "\n" ^ string_of_funcdecl b) "" (snd p)

let test =
 	string_of_program (string2expr("$gcd($a, $b) {
 	int $z;
 	$z = 3;
 	
	while ($a != $b) {
	if ($a > $b) $a = $a - $b;
	else $b = $b - $a;
	}
	return $a;
	}
	$main()
	{
	$print($gcd(2,14));
	$print($gcd(3,15));
	$print($gcd(99,121));
	}"))