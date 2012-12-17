open Ast
open Bytecode

exception IllegalMove;;

let explode s =
  let rec f acc = function
    | -1 -> acc
    | k -> f (s.[k] :: acc) (k - 1)
  in f [] (String.length s - 1) ;;

(* Execute the program *)
let execute_prog prog =
  let stack = Array.make 16000 0
  and globals = Array.make prog.globals_size 0 in

  let rec exec fp sp pc = match prog.text.(pc) with
    Litint i  -> stack.(sp) <- i ; stack.(sp+1) <- 1; exec fp (sp+2) (pc+1) 
    (* Lit String *)
  | Litstr str -> 
    (*let trimmed = trim str in
     let split_trim = Str.split(Str.regexp "\"") trimmed  in *)
      let ascii_list = List.rev (List.map Char.code (explode str)) in
        let length = List.length ascii_list in
          let diff = 38 - length in
            let rec fill_string remaining = 
              if (remaining > 0) then 
                (stack.(sp+diff-remaining) <- 0; 
                fill_string (remaining-1))
              else exec fp (sp+40) (pc+1) in 
                let rec push_elements list index = 
                  if List.length list > 0 then
                    (stack.(sp+diff+index) <- (List.hd list);
                    push_elements (List.tl list) (index+1)) 
                  else (stack.(sp+38) <- length; stack.(sp+39) <- 2; fill_string diff) 
                in
                  push_elements ascii_list 0

    (* TODO: Are we putting type after the data onto the stack? *)
  | Drp    ->
    let var_type_id = stack.(sp-1) in
      (match var_type_id with
          1 -> exec fp (sp-2) (pc+1)
        | 2 -> exec fp (sp-40) (pc+1)
        | 3 -> exec fp (sp-7) (pc+1)
        | 4 -> exec fp (sp-6) (pc+1)
        | 5 -> exec fp (sp-4) (pc+1)
        | _ -> raise(Failure("Unmatched type!!")))
      )
  | Bin op -> let op1 = stack.(sp-4) and op2 = stack.(sp-2) in     
      stack.(sp-4) <- (let boolean i = if i then 1 else 0 in
      match op with
	      Add     -> op1 + op2
      | Sub     -> op1 - op2
      | Mult    -> op1 * op2
      | Div     -> op1 / op2
      | Mod     -> op1 % op2
      | Equal   -> boolean (op1 =  op2)
      | Neq     -> boolean (op1 != op2)
      | Less    -> boolean (op1 <  op2)
      | Leq     -> boolean (op1 <= op2)
      | Greater -> boolean (op1 >  op2)
      | Geq     -> boolean (op1 >= op2)
      | And     -> (if op1 == 0 || op2 == 0 then 0 else 1)
      | Or      -> (if op1 == 1 || op2 == 1 then 1 else 0));
      exec fp (sp-1) (pc+1)

  | Lod index ->
    let var_type_id = globals.(index) in
      (match var_type_id with
        1 -> 
          stack.(sp) <- globals.(index-1);
          stack.(sp+1) <- globals.(index);
          exec fp (sp+2) (pc+1)
      | 2 ->
          for i=0 to 39 do
            stack.(sp+i) <- globals.(index-39+i)
          done;
          exec fp (sp+40) (pc+1)
      | 3 ->  (* Brick *)
          stack.(sp) <- globals.(index-6);
          stack.(sp+1) <- globals.(index-5);
          stack.(sp+2) <- globals.(index-4);
          stack.(sp+3) <- globals.(index-3);
          stack.(sp+4) <- globals.(index-2);
          stack.(sp+5) <- globals.(index-1);
          stack.(sp+6) <- globals.(index);
          exec fp (sp+7) (pc+1)
      | 4 ->  (* Player *)
          stack.(sp) <- globals.(index-5);
          stack.(sp+1) <- globals.(index-4);
          stack.(sp+2) <- globals.(index-3);
          stack.(sp+3) <- globals.(index-2);
          stack.(sp+4) <- globals.(index-1);
          stack.(sp+5) <- globals.(index);
          exec fp (sp+6) (pc+1)
      | 5 ->  (* Map *)
          stack.(sp) <- globals.(index-3);
          stack.(sp+1) <- globals.(index-2);
          stack.(sp+2) <- globals.(index-1);
          stack.(sp+3) <- globals.(index);
          exec fp (sp+4) (pc+1)
      | _ -> raise(Failure("Unmatched type!!"))
    ) 

  | Loda array_address ->
    if (stack.(sp-1) <> 1) then raise(Failure("Array index must be an int")) else
    let elem_index = stack.(sp-2) in
    let var_type_id = globals.(array_address) in
    let elem_size = (match var_type_id with
          1 -> 2
        | 2 -> 40
        | 3 -> 7
        | 4 -> 6
        | 5 -> 4 
        | _ -> (ignore(print_endline(string_of_int(globals.(array_address)))); raise(Failure("TTType check error!")))) 
    in
    (match var_type_id with
        1 -> 
          stack.(sp) <- globals.(array_address-1-elem_size*elem_index);
          stack.(sp+1) <- globals.(array_address-elem_size*elem_index);
          exec fp (sp+2) (pc+1)
          
      | 2 ->
          for i=0 to 39 do
            stack.(sp+i) <- globals.(array_address-(39-i)-elem_size*elem_index)
          done;
          exec fp (sp+40) (pc+1)
      
      | 3 -> 
          stack.(sp) <- globals.(array_address-6-elem_size*elem_index);
          stack.(sp+1) <- globals.(array_address-5-elem_size*elem_index);
          stack.(sp+2) <- globals.(array_address-4-elem_size*elem_index);
          stack.(sp+3) <- globals.(array_address-3-elem_size*elem_index);
          stack.(sp+4) <- globals.(array_address-2-elem_size*elem_index);
          stack.(sp+5) <- globals.(array_address-1-elem_size*elem_index);
          stack.(sp+6) <- globals.(array_address-elem_size*elem_index);
          exec fp (sp+7) (pc+1)
      | 4 -> 
          stack.(sp) <- globals.(array_address-5-elem_size*elem_index);
          stack.(sp+1) <- globals.(array_address-4-elem_size*elem_index);
          stack.(sp+2) <- globals.(array_address-3-elem_size*elem_index);
          stack.(sp+3) <- globals.(array_address-2-elem_size*elem_index);
          stack.(sp+4) <- globals.(array_address-1-elem_size*elem_index);
          stack.(sp+5) <- globals.(array_address-elem_size*elem_index);
          exec fp (sp+6) (pc+1)
      | 5 -> 
          stack.(sp) <- globals.(array_address-3-elem_size*elem_index);
          stack.(sp+1) <- globals.(array_address-2-elem_size*elem_index);
          stack.(sp+2) <- globals.(array_address-1-elem_size*elem_index);
          stack.(sp+3) <- globals.(array_address-elem_size*elem_index);
          exec fp (sp+4) (pc+1) 
      | _ -> raise(Failure("Unmatched type"))
    ) 

  | Str index   ->
    let var_type_id = stack.(sp-1) in
    (match var_type_id with
        1 -> 
          globals.(index-1) <- stack.(sp-2);
          globals.(index) <- stack.(sp-1);
          exec fp (sp) (pc+1)
      | 2 -> 
          for i=0 to 39 do
            globals.(index-39+i) <- stack.(sp-40+i)
          done;
          exec fp (sp) (pc+1)  
      | 3 -> 
          globals.(index-6) <- stack.(sp-7);
          globals.(index-5) <- stack.(sp-6);
          globals.(index-4) <- stack.(sp-5);
          globals.(index-3) <- stack.(sp-4);
          globals.(index-2) <- stack.(sp-3);
          globals.(index-1) <- stack.(sp-2);
          globals.(index) <- stack.(sp-1);
          exec fp (sp) (pc+1)     
      | 4 -> 
          globals.(index-5) <- stack.(sp-6);
          globals.(index-4) <- stack.(sp-5);
          globals.(index-3) <- stack.(sp-4);
          globals.(index-2) <- stack.(sp-3);
          globals.(index-1) <- stack.(sp-2);
          globals.(index) <- stack.(sp-1);
          exec fp (sp) (pc+1)
      | 5 -> 
          globals.(index-3) <- stack.(sp-4);
          globals.(index-2) <- stack.(sp-3);
          globals.(index-1) <- stack.(sp-2);
          globals.(index) <- stack.(sp-1);
          exec fp (sp) (pc+1)
      | _ -> raise(Failure("Unmatched type!!")))
  
  | Lfp i   -> stack.(sp)   <- stack.(fp+i) ; exec fp (sp+1) (pc+1)
  | Sfp i   -> stack.(fp+i) <- stack.(sp-1) ; exec fp sp     (pc+1)
  | Jsr(-1) -> print_endline (string_of_int stack.(sp-1)) ; exec fp sp (pc+1)
  | Jsr i   -> stack.(sp)   <- pc + 1       ; exec fp (sp+1) i
  | Ent i   -> stack.(sp)   <- fp           ; exec sp (sp+i+1) (pc+1)
  | Rts i   -> let new_fp = stack.(fp) and new_pc = stack.(fp-1) in
               stack.(fp-i-1) <- stack.(sp-1) ; exec new_fp (fp-i) new_pc
  | Beq i   -> exec fp (sp-1) (pc + if stack.(sp-1) =  0 then i else 1)
  | Bne i   -> exec fp (sp-1) (pc + if stack.(sp-1) != 0 then i else 1)
  | Bra i   -> exec fp sp (pc+i)



  (* Lodf and Strf *)
  | OpenWin -> (* Opens graphical display *) 
      Graphics.open_graph ""; exec fp (sp) (pc+1)
  | CloseWin -> (* Closes graphical display *)
      Graphics.clear_graph (); exec fp (sp) (pc+1)

  | Move ->
      (* Get change in x, and change in y, replace x and y in object on stack *)
      let movex = stack.(sp-2) in
      let movey = stack.(sp-4) in
      let obj_id = stack.(sp-5) in
      (
          match obj_id with
              1 ->  raise (IllegalMove)
            | 2 ->  raise (IllegalMove)

            | 3 -> (* Moves x and y for brick *)
                  stack.(sp-10) <- (stack.(sp-10) + movex)
                  stack.(sp-11) <- (stack.(sp-10) + movey)
                  exec fp sp (pc+1)

            | 4 -> (* Only moves y coordinate for player *)
                  stack.(sp-10) <- (stack.(sp-10) + movey)
                  exec fp sp (pc+1)
                  
            | 5 -> raise (IllegalMove)
            | _ -> raise(Failure("Unmatched type!!")))

  
  | Hlt     -> ()

  in exec 0 0 0
