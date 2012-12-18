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
  let stack = Array.make 16384 0
  and globals = Array.make prog.num_globals 0 in

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
  | Drp ->
    let var_type_id = stack.(sp-1) in
      (match var_type_id with
          1 -> exec fp (sp-2) (pc+1)
        | 2 -> exec fp (sp-40) (pc+1)
        | 3 -> exec fp (sp-7) (pc+1)
        | 4 -> exec fp (sp-6) (pc+1)
        | 5 -> exec fp (sp-4) (pc+1)
        | _ -> raise(Failure("Unmatched type!!")))
      
  | Bin op -> 
      let op1 = stack.(sp-4) and op2 = stack.(sp-2) in     
      stack.(sp-4) <- (let boolean i = if i then 1 else 0 in
      match op with
	      Add     -> op1 + op2
      | Sub     -> op1 - op2
      | Mult    -> op1 * op2
      | Div     -> op1 / op2
      | Mod     -> op1 mod op2
      | Equal   -> boolean (op1 =  op2)
      | Neq     -> boolean (op1 != op2)
      | Less    -> boolean (op1 <  op2)
      | Leq     -> boolean (op1 <= op2)
      | Greater -> boolean (op1 >  op2)
      | Geq     -> boolean (op1 >= op2)
      | And     -> (if op1 == 0 || op2 == 0 then 0 else 1)
      | Or      -> (if op1 == 1 || op2 == 1 then 1 else 0));
      exec fp (sp-2) (pc+1)
  | Lod i -> (* Load a global variable *)
    let var_type_id = try globals.(i) 
                      with Invalid_argument ("i out of bounds") -> -1 in
      if (var_type_id = -1) then raise (Failure("Error: Attempt to load undeclared global variable."))
      else
        (match var_type_id with
            1 -> (* int *)
              stack.(sp) <- globals.(i-1);
              stack.(sp+1) <- globals.(i);
              exec fp (sp+2) (pc+1)
          | 2 -> (* string *)
              for j=0 to 39 do
                stack.(sp+j) <- globals.(i-39+j)
              done;
              exec fp (sp+40) (pc+1)
          | 3 ->  (* Brick *)
              stack.(sp) <- globals.(i-6);
              stack.(sp+1) <- globals.(i-5);
              stack.(sp+2) <- globals.(i-4);
              stack.(sp+3) <- globals.(i-3);
              stack.(sp+4) <- globals.(i-2);
              stack.(sp+5) <- globals.(i-1);
              stack.(sp+6) <- globals.(i);
              exec fp (sp+7) (pc+1)
          | 4 ->  (* Player *)
              stack.(sp) <- globals.(i-5);
              stack.(sp+1) <- globals.(i-4);
              stack.(sp+2) <- globals.(i-3);
              stack.(sp+3) <- globals.(i-2);
              stack.(sp+4) <- globals.(i-1);
              stack.(sp+5) <- globals.(i);
              exec fp (sp+6) (pc+1)
          | 5 ->  (* Map *)
              stack.(sp) <- globals.(i-3);
              stack.(sp+1) <- globals.(i-2);
              stack.(sp+2) <- globals.(i-1);
              stack.(sp+3) <- globals.(i);
              exec fp (sp+4) (pc+1)
          | 6 ->  (* Arrayint *)
              for j=0 to 200 do
                stack.(sp+j) <- globals.(i-200+j)
              done;
              exec fp (sp+201) (pc+1)
          | 7 ->  (* Arraystring *)
              for j=0 to 4000 do
                stack.(sp+j) <- globals.(i-4000+j)
              done;
              exec fp (sp+4001) (pc+1)
          | 8 ->  (* ArrayBrick *)
              for j=0 to 700 do
                stack.(sp+j) <- globals.(i-700+j)
              done;
              exec fp (sp+701) (pc+1)
          | 9 ->  (* ArrayPlayer *)
              for j=0 to 600 do
                stack.(sp+j) <- globals.(i-600+j)
              done;
              exec fp (sp+601) (pc+1)
          | 10 ->  (* ArrayMap *)
              for j=0 to 400 do
                stack.(sp+j) <- globals.(i-400+j)
              done;
              exec fp (sp+401) (pc+1)
          | _ -> raise(Failure("Type error: Attempt to load unknown type!"))
        ) 
  | Str i -> (* Store a global variable variable *)
    let var_type_id = stack.(sp-1) in
    (
      match var_type_id with
        1 -> (* int *)
          globals.(i-1) <- stack.(sp-2);
          globals.(i) <- stack.(sp-1);
          exec fp (sp) (pc+1)
      | 2 -> (* string *)
          for j=0 to 39 do
            globals.(i-39+j) <- stack.(sp-40+j)
          done;
          exec fp (sp) (pc+1)  
      | 3 -> (* Brick *)
          for j=0 to 6 do
            globals.(i-6+j) <- stack.(sp-7+j)
          done;
          exec fp (sp) (pc+1)     
      | 4 -> (* Player *)
          for j=0 to 5 do
            globals.(i-5+j) <- stack.(sp-6+j)
          done;
          exec fp (sp) (pc+1)
      | 5 -> (* Map *)
          for j=0 to 3 do
            globals.(i-3+j) <- stack.(sp-4+j)
          done;
          exec fp (sp) (pc+1)
      | 6 -> (* Arrayint *)
          for j=0 to 200 do
            globals.(i-200+j) <- stack.(sp-201+j)
          done;
          exec fp (sp) (pc+1)
      | 7 -> (* Arraystring *)
          for j=0 to 4000 do
            globals.(i-4000+j) <- stack.(sp-4001+j)
          done;
          exec fp (sp) (pc+1)
      | 8 -> (* ArrayBrick *)
          for j=0 to 700 do
            globals.(i-700+j) <- stack.(sp-701+j)
          done;
          exec fp (sp) (pc+1)
      | 9 -> (* ArrayPlayer *)
          for j=0 to 600 do
            globals.(i-600+j) <- stack.(sp-601+j)
          done;
          exec fp (sp) (pc+1)
      | 10 -> (* ArrayMap *)
          for j=0 to 400 do
            globals.(i-400+j) <- stack.(sp-401+j)
          done;
          exec fp (sp) (pc+1)
      | _ -> raise(Failure("Type error: Unable to store variable of unknown type!"))
    )
  | Loda i -> (* Load an index of a global array *)
    if (stack.(sp-1) <> 1) then raise(Failure("Type error: Array index must be an integer!")) else
    let elem_index = stack.(sp-2) in
    let var_type_id = globals.(i) in
    let elem_size = 
      (
        match var_type_id with
          6 -> 2
        | 7 -> 40
        | 8 -> 7
        | 9 -> 6
        | 10 -> 4 
        | 0 -> raise(Failure("Attempt to access uninitialized global array."))
        | _ -> raise(Failure("Type error: Attempt to access the index of a nonarray."))
      )
    in
    (match var_type_id with
        6 -> (* Arrayint *)
          stack.(sp) <- globals.(i-2-elem_size*elem_index);
          stack.(sp+1) <- globals.(i-1-elem_size*elem_index);
          exec fp (sp+2) (pc+1)
      | 7 -> (* Arraystring *)
          for j=0 to 39 do
            stack.(sp+j) <- globals.(i-40-elem_size*elem_index+j)
          done;
          exec fp (sp+40) (pc+1)
      | 8 -> (* ArrayBrick *)
          for j=0 to 6 do
            stack.(sp+j) <- globals.(i-7-elem_size*elem_index+j)
          done;
          exec fp (sp+7) (pc+1)
      | 9 -> (* ArrayPlayer *)
          for j=0 to 5 do
            stack.(sp+j) <- globals.(i-6-elem_size*elem_index+j)
          done;
          exec fp (sp+6) (pc+1)
      | 10 -> (* ArrayMap *)
          for j=0 to 3 do
            stack.(sp+j) <- globals.(i-4-elem_size*elem_index+j)
          done;
          exec fp (sp+4) (pc+1) 
      | _ -> raise(Failure("Type error: Global variable accessed is of unknown type!"))
    )
  | Stra i -> (* Store a value into global array index from top of stack *)
    if (stack.(sp-1) <> 1) then raise(Failure("Array type check failure!")) else
    let obj_id = stack.(sp-3) 
    and loffset = stack.(sp-2)
    and var_type_id = globals.(i) in
    let elem_type =
    (
      match var_type_id with
        6 -> 1  (* Arrayint *)
      | 7 -> 2 (* Arraystring *)
      | 8 -> 3  (* ArrayBrick *)
      | 9 -> 4  (* ArrayPlayer *)
      | 10 -> 5 (* ArrayMap *)
      | 0 -> raise(Failure("Global array referenced is uninitialized."))   (* Uninitialized *)
      | _ -> raise(Failure("Type error: Global array referenced is of unknown type."))   (* Unmatched type *)
    )
    in
    if (obj_id <> elem_type) then raise (Failure("Attempt to set index of array to improper type."))
    else
    ( 
      match var_type_id with
        6 -> 
          for j=1 to 2 do
            globals.(i-j-2*loffset) <- stack.(sp-j-2)
          done;
          exec fp (sp) (pc+1)
      | 7 -> (* Arraystring *)
          for j=1 to 40 do
            globals.(i-j-40*loffset) <- stack.(sp-j-2)
          done;
          exec fp (sp) (pc+1)
      | 8 -> (* ArrayBrick *)
          for j=1 to 7 do
            globals.(i-j-7*loffset) <- stack.(sp-j-2)
          done;
          exec fp (sp) (pc+1)
      | 9 -> (* ArrayPlayer *)
          for j=1 to 6 do
            globals.(i-j-6*loffset) <- stack.(sp-j-2)
          done;
          exec fp (sp) (pc+1)
      | 10 -> (* ArrayMap *)
          for j=1 to 4 do
            globals.(i-j-4*loffset) <- stack.(sp-j-2)
          done;
          exec fp (sp) (pc+1)
      | _ -> raise(Failure("Type error: Attempt to store array of unknown type."))
    )
  | Lfp i -> (* Load a local variable *)
      let var_type_id = stack.(fp+i) in
      (
          match var_type_id with
            1 -> (* int *)
              stack.(sp) <- stack.(fp+i-1);
              stack.(sp+1) <- stack.(fp+i);
              exec fp (sp+2) (pc+1)
          | 2 -> (* string *)
              for j=0 to 39 do
                stack.(sp+j) <- stack.(fp+i-39+j)
              done;
              exec fp (sp+40) (pc+1)
          | 3 -> (* Brick *)
              for j=0 to 6 do
                stack.(sp+j) <- stack.(fp+i-6+j)
              done;
              exec fp (sp+7) (pc+1)
          | 4 -> (* Player *)
              for j=0 to 5 do
                stack.(sp+j) <- stack.(fp+i-5+j)
              done;
              exec fp (sp+6) (pc+1)
          | 5 -> (* Map *)
              for j=0 to 3 do
                stack.(sp+j) <- stack.(fp+i-3+j)
              done;
              exec fp (sp+4) (pc+1)
          | 6 -> (* Arrayint *)
              for j=0 to 200 do
                stack.(sp+j) <- stack.(fp+i-200+j)
              done;
              exec fp (sp+201) (pc+1)
          | 7 -> (* Arraystring *)
              for j=0 to 4000 do
                stack.(sp+j) <- stack.(fp+i-4000+j)
              done;
              exec fp (sp+4001) (pc+1)
          | 8 -> (* ArrayBrick *)
              for j=0 to 700 do
                stack.(sp+j) <- stack.(fp+i-700+j)
              done;
              exec fp (sp+701) (pc+1)
          | 9 -> (* ArrayPlayer *)
              for j=0 to 600 do
                stack.(sp+j) <- stack.(fp+i-600+j)
              done;
              exec fp (sp+601) (pc+1)
          | 10 -> (* ArrayMap *)
              for j=0 to 400 do
                stack.(sp+j) <- stack.(fp+i-400+j)
              done;
              exec fp (sp+401) (pc+1)  
          | 0 -> (* Uninitialized variable *)
              raise(Failure("Attempt to load uninitialized local variable."))  
          | _ -> raise(Failure("Type error: Attempt to load variable of unknown type.")))
      
  | Sfp i   -> 
      let obj_id = stack.(sp-1) in
      ( 
        match obj_id with
          1 -> (* int *)
            stack.(fp+i) <- stack.(sp-1); 
            stack.(fp+i-1) <- stack.(sp-2); 
            exec fp (sp) (pc+1)
        | 2 -> (* string *)
            for j=0 to 39 do
              stack.(fp+i-j) <- stack.(sp-j-1)
            done;
            exec fp (sp) (pc+1)

        | 3 -> (* Brick *)
            for j=0 to 6 do
              stack.(fp+i-j) <- stack.(sp-j-1)
            done;
            exec fp (sp) (pc+1)
        | 4 -> (* Player *)
            for j=0 to 5 do
              stack.(fp+i-j) <- stack.(sp-j-1)
            done;
            exec fp (sp) (pc+1)
        | 5 -> (* Map *)
            for j=0 to 3 do
              stack.(fp+i-j) <- stack.(sp-j-1)
            done;
            exec fp (sp) (pc+1)
        | 6 -> (* Arrayint *)
            for j=0 to 200 do
              stack.(fp+i-j) <- stack.(sp-j-1)
            done;
            exec fp (sp) (pc+1)
        | 7 -> (* Arraystring *)
            for j=0 to 4000 do
              stack.(fp+i-j) <- stack.(sp-j-1)
            done;
            exec fp (sp) (pc+1)
        | 8 -> (* ArrayBrick *)
            for j=0 to 700 do
              stack.(fp+i-j) <- stack.(sp-j-1)
            done;
            exec fp (sp) (pc+1)
        | 9 -> (* ArrayPlayer *)
            for j=0 to 600 do
              stack.(fp+i-j) <- stack.(sp-j-1)
            done;
            exec fp (sp) (pc+1)
        | 10 -> (* ArrayMap *)
            for j=0 to 400 do
              stack.(fp+i-j) <- stack.(sp-j-1)
            done;
            exec fp (sp) (pc+1)
        | _ -> raise(Failure("Type error: Unmatched type error!"))
      )
  | Lfpa i -> (* Load index of local array, based on next integer on stack *)
    if (stack.(sp-1) <> 1 ) then raise(Failure("Type error: Array index must be an integer.")) else
    let obj_id = stack.(fp+i)
    and loffset = stack.(sp-2) in
    ( 
      match obj_id with
        6 -> (* Arrayint *)
          stack.(sp) <- stack.(fp+i-2-loffset*2);
          stack.(sp+1) <- stack.(fp+i-1-loffset*2);
          exec fp (sp+2) (pc+1)
      | 7 -> (* Arraystring *)
          for j=0 to 39 do
            stack.(sp+j) <- stack.(fp+i-40+j)
          done;
          exec fp (sp+40) (pc+1)
      | 8 -> (* ArrayBrick *)
          for j=0 to 6 do
            stack.(sp+j) <- stack.(fp+i-7+j)
          done;
          exec fp (sp+7) (pc+1)
      | 9 -> (* ArrayPlayer *)
          for j=0 to 5 do
            stack.(sp+j) <- stack.(fp+i-6+j)
          done;
          exec fp (sp+5) (pc+1)
      | 10 -> (* ArrayMap *)
          for j=0 to 3 do
            stack.(sp+j) <- stack.(fp+i-4+j)
          done;
          exec fp (sp+4) (pc+1)
      | 0 -> (* Uninitialized array *)
          raise(Failure("Attempt to access index of uninitialized array."))
      | _ -> raise(Failure("Type error: Attempt to access index of array of unknown type."))
    )
  | Sfpa i -> (* Store into index of array the next item on stack after index *)
    if (stack.(sp-1) <> 1) then raise(Failure("Type error: Array index must be an integer.")) else 
    let obj_id = stack.(sp-3) 
    and loffset = stack.(sp-2)
    and array_type_id = stack.(fp+i) in
    if (obj_id <> (match array_type_id with
                      6 -> 1
                    | 7 -> 2
                    | 8 -> 3
                    | 9 -> 4
                    | 10 -> 5
                    | 0 -> raise (Failure("Attempt to store value into uninitialized array."))
                    | _ -> raise (Failure("Type error: Attempt to access array of unknown type."))))
      then raise(Failure("Type mismatch: Attempt to store value of mismatched type into local array."))
    else
    ( 
      match array_type_id with
        6 -> (* Arrayint *)
           stack.(fp+i-1-2*loffset) <- stack.(sp-1-2); 
           stack.(fp+i-2-2*loffset) <- stack.(sp-2-2); 
           exec fp (sp) (pc+1)
      | 7 -> (* Arraystring *)
          for j=1 to 40 do
            stack.(fp+i-j-40*loffset) <- stack.(sp-j-2)
          done;
          exec fp (sp) (pc+1)
      | 8 -> (* ArrayBrick *)
          for j=1 to 7 do
            stack.(fp+i-j-7*loffset) <- stack.(sp-j-2)
          done;
          exec fp (sp) (pc+1)
      | 9 -> (* ArrayPlayer *)
          for j=1 to 6 do
            stack.(fp+i-j-6*loffset) <- stack.(sp-j-2)
          done;
          exec fp (sp) (pc+1)
      | 10 -> (* ArrayMap *)
          for j=1 to 4 do
            stack.(fp+i-j-4*loffset) <- stack.(sp-j-2)
          done;
          exec fp (sp) (pc+1)
      | _ -> raise(Failure("Type error: Attempt to store value into array of unknown type."))
    )
  | Jsr(-1) -> (* draw *) 
      print_endline "You've just drawn something!" ; exec fp sp (pc+1)
  | Jsr(-2) -> (* run *)
      print_endline "You've just started running your program!" ; exec fp sp (pc+1)
  | Jsr(-3) -> (* printint *)
      print_endline (string_of_int stack.(sp-2)) ; exec fp sp (pc+1)
  | Jsr(-4) -> (* printstring *)
      let var_type_id = stack.(sp-1) in

      if var_type_id <> 2 then raise (Failure("Type error: Unable to call printstring on nonstring."))
      else let strLen = stack.(sp-2) in
              let rec buildStr remaining str = if (remaining > 0) then
                  buildStr (remaining-1) ((Char.escaped (char_of_int (stack.(sp-remaining-2)))) ^ str) else
                  str in
                  print_endline (buildStr strLen "");
                  exec fp (sp) (pc+1)

  | Jsr(-5) -> (* printarray *)
      let var_type_id = stack.(sp-1) in
      (
          match var_type_id with
              6 ->  (* Arrayint *)
                let rec printnextint p =
                  let var_type = stack.(sp-p) in 
                  ( 
                    match var_type with
                        1 -> print_endline (string_of_int (stack.(sp-p-1)))
                    |   0 -> print_endline "Array has not been initialized."
                    |   _ -> printnextint (sp-2)
                  ) in
                  printnextint 2;
          |   7 ->  (* Arraystring *)
                let rec printnextint p =
                  let var_type = stack.(sp-p) in 
                  ( 
                    match var_type with
                        2 -> let strLen = stack.(sp-p-2) in
                                let rec buildStr remaining stp str = if (remaining > 2) then
                                    buildStr (remaining-1) stp ((Char.escaped (char_of_int (stack.(stp-remaining-2)))) ^ str) else
                                    str in
                                    print_endline (buildStr strLen (sp-p) "")
                    |   0 -> print_endline "Array has not been initialized."
                    |   _ -> printnextint (sp-40)
                  ) in
                  printnextint 2;
        (*  |   8 ->  (* ArrayBrick *)
            |   9 ->  (* ArrayPlayer *)
            |   10 ->  ArrayMap *)
      )
  | Jsr(-6) -> (* dumpstack *)
      Array.iter print_endline (Array.map string_of_int stack); 
 (* | Jsr(-7) ->  push *)
  | Jsr i   -> stack.(sp) <- (pc + 1) ; exec fp (sp+1) i
  | Ent i   -> stack.(sp)   <- fp           ; exec sp (sp+i+1) (pc+1)
  | Rts i   -> let new_fp = stack.(fp) and new_pc = stack.(fp-1) in
               stack.(fp-i-1) <- stack.(sp-1) ; exec new_fp (fp-i) new_pc
  | Beq i   -> exec fp (sp-1) (pc + if stack.(sp-2) =  0 then i else 1)
  | Bne i   -> exec fp (sp-1) (pc + if stack.(sp-1) != 0 then i else 1)
  | Bra i   -> exec fp sp (pc+i)

  (* Lodf and Strf *)
  | OpenWin -> (* Opens graphical display *) 
      Graphics.open_graph ""; exec fp (sp) (pc+1)
  | CloseWin -> (* Closes graphical display *)
      Graphics.clear_graph (); exec fp (sp) (pc+1)

  | MakeB ->
        let v1 = stack.(sp-1)  (* 3 *)
        and v2 = stack.(sp-3)  (* R *)
        and v3 = stack.(sp-5)  (* G *)
        and v4 = stack.(sp-7)  (* B *)
        and v5 = stack.(sp-9)  (* varray address *)
        and v6 = stack.(sp-11) (* x *)
        and v7 = stack.(sp-13) (* y *)

        in
        if ((v1 <> 1) || (v2 <> 1) || (v3 <> 1) || (v4 <> 1) || (v5 <> 1) || (v6 <> 1) || (v7 <> 1)) then 
          raise(Failure("MakeB type check error!")) else
          ( 
            stack.(sp-1) <- stack.(sp-2);
            stack.(sp-2) <- stack.(sp-4);
            stack.(sp-3) <- stack.(sp-6);
            stack.(sp-4) <- stack.(sp-8);
            stack.(sp-5) <- stack.(sp-10);
            stack.(sp-6) <- stack.(sp-12);
            stack.(sp-7) <- stack.(sp-14);
            exec fp sp (pc+1)
          )

  | MakeP ->      
        let v1 = stack.(sp-1)  (* 4 *)
        and v2 = stack.(sp-3)  (* R *)
        and v3 = stack.(sp-5)  (* G *)
        and v4 = stack.(sp-7)  (* B *)
        and v5 = stack.(sp-9)  (* varray address *)
        and v6 = stack.(sp-11) (* y *)

        in
        if ((v1 <> 1) || (v2 <> 1) || (v3 <> 1) || (v4 <> 1) || (v5 <> 1) || (v6 <> 1)) then 
          raise(Failure("MakeB type check error!")) else
          ( 
            stack.(sp-1) <- stack.(sp-2);
            stack.(sp-2) <- stack.(sp-4);
            stack.(sp-3) <- stack.(sp-6);
            stack.(sp-4) <- stack.(sp-8);
            stack.(sp-5) <- stack.(sp-10);
            stack.(sp-6) <- stack.(sp-12);
            stack.(sp-7) <- stack.(sp-14);
            exec fp sp (pc+1)
          )

  | MakeM ->
        let v1 = stack.(sp-1)  (* 5 *)
        and v2 = stack.(sp-3)  (* generator ID address *)
        and v3 = stack.(sp-5)  (* height *)
        and v4 = stack.(sp-7)  (* width *)

        in
        if ((v1 <> 1) || (v2 <> 1) || (v3 <> 1) || (v4 <> 1)) then 
          raise(Failure("MakeB type check error!")) else
          ( 
            stack.(sp-1) <- stack.(sp-2);
            stack.(sp-2) <- stack.(sp-4);
            stack.(sp-3) <- stack.(sp-6);
            stack.(sp-4) <- stack.(sp-8);
            exec fp sp (pc+1)
          )

(*  | Move ->
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
            | _ -> raise(Failure("Unmatched type!!"))) *)

  
  | Hlt     -> ()

  in exec 0 0 0
