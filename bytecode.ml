type bstmt =
    Litint of int         (* Push a int literal *)
  | Litstr of string      (* Push a string literal *)
  | Drp                   (* Discard a value *)
  | Bin of Ast.op         (* Perform arithmetic on top of stack *)
  | Lod of int            (* Fetch global variable *)
  | Str of int            (* Store global variable *)
  | Loda                  (* Load global array variable *)
  | Stra                  (* Stores global array variable *)
  | Lfp of int            (* Load frame pointer relative *)
  | Sfp of int            (* Store frame pointer relative *)
  | Lfpa                  (* This is the start index of this array variable. Index is evaluated and 
                             put on top of stack in an int structure. *)
  | Sfpa                  (* Stores frame pointer of array *)
  | Jsr of int            (* Call function by absolute address *)
  | Ent of int            (* Push FP, FP -> SP, SP += i *)
  | Rts of int            (* Restore FP, SP, consume formals, push result *)
  | Beq of int            (* Branch relative if top-of-stack is zero *)
  | Bne of int            (* Branch relative if top-of-stack is non-zero *)
  | Bra of int            (* Branch relative *)
  | OpenWin               (* Opens a display window *)
  | CloseWin              (* Closes the display window *)
  | DrawPlayer            (* Draws a player object on top of the stack *)
  | CheckCollision        (* Checks if the player object has collided with anyone *)
  | CheckUserInput        (* Checks for user input and modifies player on stack *)
  | Hlt                   (* Terminate *)
  | Nt

type prog = {
    globals_size : int;   (* Number of global variables *)
    text : bstmt array; (* Code for all the functions *)
  }

let string_of_stmt = function
    Litint(i) -> "Litint " ^ string_of_int i
  | Litstr(i) -> "Litstr " ^ i
  | Drp -> "Drp"
  | Bin(Ast.Add) -> "Add"
  | Bin(Ast.Sub) -> "Sub"
  | Bin(Ast.Mult) -> "Mul"
  | Bin(Ast.Div) -> "Div"
  | Bin(Ast.Mod) -> "Mod"
  | Bin(Ast.Equal) -> "Eql"
  | Bin(Ast.Neq) -> "Neq"
  | Bin(Ast.Less) -> "Lt"
  | Bin(Ast.Leq) -> "Leq"
  | Bin(Ast.Greater) -> "Gt"
  | Bin(Ast.Geq) -> "Geq"
  | Bin(Ast.And) -> "And"
  | Bin(Ast.Or) -> "Or"
  | Lod(i) -> "Lod " ^ string_of_int i
  | Str(i) -> "Str " ^ string_of_int i
  | Lfp(i) -> "Lfp " ^ string_of_int i
  | Sfp(i) -> "Sfp " ^ string_of_int i
  | Jsr(i) -> "Jsr " ^ string_of_int i
  | Ent(i) -> "Ent " ^ string_of_int i
  | Rts(i) -> "Rts " ^ string_of_int i
  | Bne(i) -> "Bne " ^ string_of_int i
  | Beq(i) -> "Beq " ^ string_of_int i
  | Bra(i) -> "Bra " ^ string_of_int i
  | Lfpa -> "Lfpa"
  | Sfpa -> "Sfpa"
  | Loda -> "Loda"
  | Stra -> "Stra"
  | CheckCollision -> "CheckCollision"
  | CheckUserInput -> "CheckUserInput"
  | DrawPlayer -> "DrawPlayer"
  | OpenWin -> "OpenWin"
  | CloseWin -> "CloseWin"
  | Nt -> "Not"
  | Hlt    -> "Hlt"

let string_of_prog p =
  string_of_int p.globals_size ^ " global variables\n" ^
  let funca = Array.mapi
      (fun i s -> string_of_int i ^ " " ^ string_of_stmt s) p.text
  in String.concat "\n" (Array.to_list funca)
