module StringMap = Map.Make(String)

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
  | Lref                  (* Loads a value onto the stack from an address *)
  | Sref                  (* Saves a value from the top of the stack into an address *)
  | Jsr of int            (* Call function by absolute address *)
  | Ent of int            (* Push FP, FP -> SP, SP += i *)
  | Rts of int            (* Restore FP, SP, consume formals, push result *)
  | Beq of int            (* Branch relative if top-of-stack is zero *)
  | Bne of int            (* Branch relative if top-of-stack is non-zero *)
  | Bra of int            (* Branch relative *)
  | Make of int           (* Shift stack pointer by 1 for Player, Map, Brick; Adds vartype_id to first space in arrays *)
  | Init of int * int * int
  | Litf of int
  | OpenWin               (* Opens a display window *)
  | CloseWin              (* Closes the display window *)
  | StoreWindow           (* Store Width and Height or the window *)
  | DrawPlayer            (* Draws a player object on top of the stack *)
  | ProcessBlocks
  | CheckCollision        (* Checks if the player object has collided with anyone *)
  | CheckUserInput        (* Checks for user input and modifies player on stack *)
  | PrintScore            (* Prints the user's current score on the top left *)
  | Hlt                   (* Terminate *)
  | Nt                    (* Negate 1 or 0 on top of stack *)

type prog = {
    globals_size : int;   (* Number of global variables *)
    text : bstmt array;   (* Code for all the functions *)
  }

let string_of_stmt = function
    Litint(i) -> "Litint " ^ string_of_int i
  | Litstr(i) -> "Litstr " ^ i
  | Litf(i) -> "Litf " ^ string_of_int i
  | Drp -> "Drp"
  | Bin(Ast.Add) -> "Bin Add"
  | Bin(Ast.Sub) -> "Bin Sub"
  | Bin(Ast.Mult) -> "Bin Mul"
  | Bin(Ast.Div) -> "Bin Div"
  | Bin(Ast.Mod) -> "Bin Mod"
  | Bin(Ast.Equal) -> "Bin Eql"
  | Bin(Ast.Neq) -> "Bin Neq"
  | Bin(Ast.Less) -> "Bin Lt"
  | Bin(Ast.Leq) -> "Bin Leq"
  | Bin(Ast.Greater) -> "Bin Gt"
  | Bin(Ast.Geq) -> "Bin Geq"
  | Bin(Ast.And) -> "Bin And"
  | Bin(Ast.Or) -> "Bin Or"
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
  | Make(i)   -> "Make " ^ string_of_int i
  | Init(i, j, k) -> "Init " ^ (string_of_int i) ^ " " ^ (string_of_int j) ^ " " ^ (string_of_int k)
  | Lfpa -> "Lfpa"
  | Sfpa -> "Sfpa"
  | Loda -> "Loda"
  | Stra -> "Stra"
  | Lref -> "Lref"
  | Sref -> "Sref"
  | CheckCollision -> "CheckCollision"
  | CheckUserInput -> "CheckUserInput"
  | DrawPlayer -> "DrawPlayer"
  | PrintScore -> "PrintScore"
  | ProcessBlocks -> "ProcessBlocks"
  | OpenWin -> "OpenWin"
  | CloseWin -> "CloseWin"
  | Nt -> "Not"
  | Hlt    -> "Hlt"

let string_of_prog p =
  string_of_int p.globals_size ^ " global variables\n" ^
  let funca = Array.mapi
      (fun i s -> string_of_int i ^ " " ^ string_of_stmt s) p.text
  in String.concat "\n" (Array.to_list funca)
