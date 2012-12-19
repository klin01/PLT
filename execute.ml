open Ast
open Bytecode

exception IllegalMove;;

let array_def_size = 100

let explode s =
  let rec f acc = function
    | -1 -> acc
    | k -> f (s.[k] :: acc) (k - 1)
  in f [] (String.length s - 1) ;;

let countArray stack sp =
  let rec countItems size count t =
    if stack.(sp-1-size*count) <> t then count else
      countItems size (count+1) t in 
  match stack.(sp-1) with
      6 -> countItems 2 0 1
  |   7 -> countItems 40 0 2
  |   8 -> countItems 13 0 3
  |   9 -> countItems 11 0 4
  |   10 -> countItems 7 0 5
  |   _ -> raise(Failure("Type error: Array is of unknown type."));;

(* Execute the program *)
let execute_prog prog =
  let stack = Array.make 32768 0
  and globals = Array.make prog.globals_size 0 in

  let rec exec fp sp pc = try match prog.text.(pc) with
    Litint i  -> 
    (*print_endline("litint " ^ string_of_int i);*)
    stack.(sp) <- i ; stack.(sp+1) <- 1; exec fp (sp+2) (pc+1) 
    (* Lit String *)
  | Litstr str -> 
    (*let trimmed = trim str in
     let split_trim = Str.split(Str.regexp "\"") trimmed  in *)
      let ascii_list = List.rev (List.map Char.code (explode str)) in
        let length = List.length ascii_list in
          if (length > 38) then raise(Failure("The maximum string length allowed is 38.")) else
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
        | 3 -> exec fp (sp-13) (pc+1)
        | 4 -> exec fp (sp-11) (pc+1)
        | 5 -> exec fp (sp-7) (pc+1)
        | 6 -> exec fp (sp-array_def_size*2-1) (pc+1)
        | 7 -> exec fp (sp-array_def_size*40-1) (pc+1) 
        | 8 -> exec fp (sp-array_def_size*13-1) (pc+1)
        | 9 -> exec fp (sp-array_def_size*11-1) (pc+1)
        | 10 -> exec fp (sp-array_def_size*7-1) (pc+1)
        | _ -> raise(Failure("Unmatched type in Drp!!" ^ string_of_int var_type_id)))
      
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
              for j=0 to 12 do
                stack.(sp+j) <- globals.(i-12+j)
              done;
              exec fp (sp+13) (pc+1)
          | 4 ->  (* Player *)
              for j=0 to 10 do
                stack.(sp+j) <- globals.(i-10+j)
              done;
              exec fp (sp+11) (pc+1)
          | 5 ->  (* Map *)
              for j=0 to 6 do
                stack.(sp+j) <- globals.(i-6+j)
              done;
              exec fp (sp+7) (pc+1)
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
              for j=0 to 1300 do
                stack.(sp+j) <- globals.(i-1300+j)
              done;
              exec fp (sp+1301) (pc+1)
          | 9 ->  (* ArrayPlayer *)
              for j=0 to 1100 do
                stack.(sp+j) <- globals.(i-100+j)
              done;
              exec fp (sp+1101) (pc+1)
          | 10 ->  (* ArrayMap *)
              for j=0 to 700 do
                stack.(sp+j) <- globals.(i-700+j)
              done;
              exec fp (sp+701) (pc+1)
          | _ -> print_endline(string_of_int var_type_id); raise(Failure("Type error: Attempt to load unknown type!"))
        ) 
  | Str i -> (* Store a global variable variable *)
    let var_type_id = stack.(sp-1) in
    ( (*print_endline("str type " ^ string_of_int var_type_id);*)
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
          for j=0 to 12 do
            globals.(i-12+j) <- stack.(sp-13+j)
          done;
          exec fp (sp) (pc+1)     
      | 4 -> (* Player *)
          for j=0 to 10 do
            globals.(i-10+j) <- stack.(sp-11+j)
          done;
          exec fp (sp) (pc+1)
      | 5 -> (* Map *)
          for j=0 to 6 do
            globals.(i-6+j) <- stack.(sp-7+j)
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
          for j=0 to 1300 do
            globals.(i-1300+j) <- stack.(sp-1301+j)
          done;
          exec fp (sp) (pc+1)
      | 9 -> (* ArrayPlayer *)
          for j=0 to 1100 do
            globals.(i-1100+j) <- stack.(sp-1101+j)
          done;
          exec fp (sp) (pc+1)
      | 10 -> (* ArrayMap *)
          for j=0 to 700 do
            globals.(i-700+j) <- stack.(sp-701+j)
          done;
          exec fp (sp) (pc+1)
      | 0 -> raise(Failure("Unable to store uninitialized variable."))
      | _ -> raise(Failure("Type error: Unable to store variable of unknown type."))
    )
  | Loda -> (* Load an index of a global array, first element on stack is address of array, next element is index of array *)
            (*print_endline ("loda" ^ string_of_int stack.(sp-1) ^ " " ^ string_of_int stack.(sp-2) ^ " " ^ string_of_int stack.(sp-3)
                  ^ " " ^ string_of_int stack.(sp-4) ^ " " ^ string_of_int stack.(sp-5)
                  ^ " " ^ string_of_int stack.(sp-6) ^ " " ^ string_of_int stack.(sp-7)
                ^ " " ^ string_of_int stack.(sp-8) ^ " " ^ string_of_int stack.(sp-9)) ;*)

    if (stack.(sp-1) <> 1) then raise(Failure("Invalid array address.")) else
    if (stack.(sp-3) <> 1) then raise(Failure("Type error: Array index must be an integer!")) else
    let i = stack.(sp-2)
    and elem_index = stack.(sp-4) in
    let var_type_id = globals.(i) in
    let cnst_offset = 4 in
    (*print_endline ("var type id" ^ string_of_int globals.(i));*)
    let elem_size = 
      (
        match var_type_id with
          6 -> 2  (* int *)
        | 7 -> 40 (* string *)
        | 8 -> 13 (* Brick *)
        | 9 -> 11 (* Player *)
        | 10 -> 7 (* Map *)
        | 0 -> raise(Failure("Attempt to access uninitialized global array."))
        | _ -> raise(Failure("Type error: Attempt to access the index of a nonarray."))
      )
    in
    (match var_type_id with
        6 -> (* Arrayint *)
          stack.(sp-4) <- globals.(i-2-elem_size*elem_index);
          stack.(sp+1-4) <- globals.(i-1-elem_size*elem_index);
          (*print_endline("loaded" ^ string_of_int stack.(sp));*)
          exec fp (sp-2) (pc+1)
      | 7 -> (* Arraystring *)
          for j=0 to 39 do
            stack.(sp+j-cnst_offset) <- globals.(i-40-elem_size*elem_index+j)
          done;
          exec fp (sp+40-cnst_offset) (pc+1)
      | 8 -> (* ArrayBrick *)
          for j=0 to 12 do
            stack.(sp+j-cnst_offset) <- globals.(i-13-elem_size*elem_index+j)
          done;
          exec fp (sp+13-cnst_offset) (pc+1)
      | 9 -> (* ArrayPlayer *)
          for j=0 to 10 do
            stack.(sp+j-cnst_offset) <- globals.(i-11-elem_size*elem_index+j)
          done;
          exec fp (sp+11-cnst_offset) (pc+1)
      | 10 -> (* ArrayMap *)
          for j=0 to 6 do
            stack.(sp+j-cnst_offset) <- globals.(i-7-elem_size*elem_index+j)
          done;
          exec fp (sp+7-cnst_offset) (pc+1) 
      | _ -> raise(Failure("Type error: Global variable accessed is of unknown type."))
    )
  | Stra -> (* Store a value into global array index, top of stack is array address, next is array index, then value to store *)
    if (stack.(sp-1) <> 1) then raise(Failure("Invalid array address.")) else
    if (stack.(sp-3) <> 1) then raise(Failure("Type error: Array index must be an integer.")) else
    let array_address = stack.(sp-2)
    and obj_id = stack.(sp-5) 
    and offset = stack.(sp-4) in
    let var_type_id = globals.(array_address) in
    let elem_type =
    (
      match var_type_id with
        6 -> 1  (* int *)
      | 7 -> 2  (* string *)
      | 8 -> 3  (* Brick *)
      | 9 -> 4  (* Player *)
      | 10 -> 5 (* Map *)
      | 0 -> raise(Failure("Global array referenced is uninitialized."))   (* Uninitialized *)
      | _ -> raise(Failure("Type error: Global array referenced is of unknown type."))   (* Unmatched type *)
    )
    in
    if (obj_id <> elem_type) then raise (Failure("Attempt to set index of array to mismatched type."))
    else
    ( 
      match var_type_id with
        6 -> (* Arrayint *)
          (* 
          globals.(array_address-2-2*offset) <- stack.(sp-2-2)
          globals.(array_address-1-2*offset) <- stack.(sp-1-2) 
          *)
          for j=1 to 2 do
            globals.(array_address-j-2*offset) <- stack.(sp-j-4)
          done;
          exec fp (sp) (pc+1)
      | 7 -> (* Arraystring *)
          for j=1 to 40 do
            globals.(array_address-j-40*offset) <- stack.(sp-j-4)
          done;
          exec fp (sp) (pc+1)
      | 8 -> (* ArrayBrick *)
          for j=1 to 13 do
            globals.(array_address-j-13*offset) <- stack.(sp-j-4)
          done;
          exec fp (sp) (pc+1)
      | 9 -> (* ArrayPlayer *)
          for j=1 to 11 do
            globals.(array_address-j-11*offset) <- stack.(sp-j-4)
          done;
          exec fp (sp) (pc+1)
      | 10 -> (* ArrayMap *)
          for j=1 to 7 do
            globals.(array_address-j-7*offset) <- stack.(sp-j-4)
          done;
          exec fp (sp) (pc+1)
      | _ -> raise(Failure("Type error: Attempt to store array of unknown type."))
    )
  | Lfp i -> (* Load a local variable *)
      let var_type_id = stack.(fp+i) in
      (
          match var_type_id with
            1 -> (* int *)
              stack.(sp) <- stack.(fp+i-1); (* value *)
              stack.(sp+1) <- stack.(fp+i); (* type *)
              exec fp (sp+2) (pc+1)
          | 2 -> (* string *)
              for j=0 to 39 do
                stack.(sp+j) <- stack.(fp+i-39+j)
              done;
              exec fp (sp+40) (pc+1)
          | 3 -> (* Brick *)
              for j=0 to 12 do
                stack.(sp+j) <- stack.(fp+i-12+j)
              done;
              exec fp (sp+13) (pc+1)
          | 4 -> (* Player *)
              for j=0 to 10 do
                stack.(sp+j) <- stack.(fp+i-10+j)
              done;
              exec fp (sp+11) (pc+1)
          | 5 -> (* Map *)
              for j=0 to 6 do
                stack.(sp+j) <- stack.(fp+i-6+j)
              done;
              exec fp (sp+7) (pc+1)
          | 6 -> (* Arrayint *)
              for j=0 to 200 do
                stack.(sp+j) <- stack.(fp+i-200+j)
              done;
              exec fp (sp+201) (pc+1)
          | 7 -> (* Arrayƒstring *)
              for j=0 to 4000 do
                stack.(sp+j) <- stack.(fp+i-4000+j)
              done;
              exec fp (sp+4001) (pc+1)
          | 8 -> (* ArrayBrick *)
              for j=0 to 1300 do
                stack.(sp+j) <- stack.(fp+i-1300+j)
              done;
              exec fp (sp+1301) (pc+1)
          | 9 -> (* ArrayPlayer *)
              for j=0 to 1100 do
                stack.(sp+j) <- stack.(fp+i-1100+j)
              done;
              exec fp (sp+1101) (pc+1)
          | 10 -> (* ArrayMap *)
              for j=0 to 700 do
                stack.(sp+j) <- stack.(fp+i-700+j)
              done;
              exec fp (sp+701) (pc+1)  
          | 0 -> (* Uninitialized variable *)
              raise(Failure("Attempt to load uninitialized local variable."))  
          | _ -> raise(Failure("Type error: Attempt to load variable of unknown type.")))
      
  | Sfp i   -> 
  (*print_endline ("sfp " ^ string_of_int stack.(sp-1) ^ " " ^ string_of_int stack.(sp-2) ^ " " ^ string_of_int stack.(sp-3)
                  ^ " " ^ string_of_int stack.(sp-4) ^ " " ^ string_of_int stack.(sp-5)
                  ^ " " ^ string_of_int stack.(sp-6) ^ " " ^ string_of_int stack.(sp-7)
                ^ " " ^ string_of_int stack.(sp-8) ^ " " ^ string_of_int stack.(sp-9)) ;*)

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
            for j=0 to 12 do
              stack.(fp+i-j) <- stack.(sp-j-1)
            done;
            exec fp (sp) (pc+1)
        | 4 -> (* Player *)
            for j=0 to 10 do
              stack.(fp+i-j) <- stack.(sp-j-1)
            done;
            exec fp (sp) (pc+1)
        | 5 -> (* Map *)
            for j=0 to 6 do
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
            for j=0 to 1300 do
              stack.(fp+i-j) <- stack.(sp-j-1)
            done;
            exec fp (sp) (pc+1)
        | 9 -> (* ArrayPlayer *)
            for j=0 to 1100 do
              stack.(fp+i-j) <- stack.(sp-j-1)
            done;
            exec fp (sp) (pc+1)
        | 10 -> (* ArrayMap *)
            for j=0 to 700 do
              stack.(fp+i-j) <- stack.(sp-j-1)
            done;
            exec fp (sp) (pc+1)
        | _ -> print_endline (string_of_int obj_id); raise(Failure("Type error: Unmatched type error!"))
      )
  | Lfpa -> (* Load index of local array, based on next integer on stack *)

       (* print_endline ("lfpa" ^ string_of_int stack.(sp-1) ^ " " ^ string_of_int stack.(sp-2) ^ " " ^ string_of_int stack.(sp-3)
                  ^ " " ^ string_of_int stack.(sp-4) ^ " " ^ string_of_int stack.(sp-5)
                  ^ " " ^ string_of_int stack.(sp-6) ^ " " ^ string_of_int stack.(sp-7)
                ^ " " ^ string_of_int stack.(sp-8) ^ " " ^ string_of_int stack.(sp-9)) ;*)
    if (stack.(sp-1) <> 1) then raise(Failure("Invalid array address.")) else
    if (stack.(sp-3) <> 1) then raise(Failure("Type error: Array index must be an integer.")) else
    let i = stack.(sp-2) in (* array address *)
    let cnst_offset = 4 in
    let obj_id = stack.(fp+i)
    and loffset = stack.(sp-4) in
    ( 
      match obj_id with
        6 -> (* Arrayint *)
          stack.(sp-4) <- stack.(fp+i-2-loffset*2); (* value *)
          stack.(sp+1-4) <- stack.(fp+i-1-loffset*2); (* type *)
          exec fp (sp-2) (pc+1)
      | 7 -> (* Arraystring *)
          for j=0 to 39 do
            stack.(sp+j-cnst_offset) <- stack.(fp+i-40+j-loffset*40)
          done;
          exec fp (sp+40-cnst_offset) (pc+1)
      | 8 -> (* ArrayBrick *)
          for j=0 to 12 do
            stack.(sp+j-cnst_offset) <- stack.(fp+i-13+j-loffset*13)
          done;
          exec fp (sp+13-cnst_offset) (pc+1)
      | 9 -> (* ArrayPlayer *)
          for j=0 to 10 do
            stack.(sp+j-cnst_offset) <- stack.(fp+i-11+j-loffset*11);
          done;
          exec fp (sp+11-cnst_offset) (pc+1)
      | 10 -> (* ArrayMap *)
          for j=0 to 6 do
            stack.(sp+j-cnst_offset) <- stack.(fp+i-7+j-loffset*7)
          done;
          exec fp (sp+7-cnst_offset) (pc+1)
      | 0 -> (* Uninitialized array *)
          raise(Failure("Attempt to access index of uninitialized array."))
      | _ -> raise(Failure("Type error: Attempt to access index of array of unknown type."))
  )
  | Sfpa -> (* Store into index of array the next item on stack after index *)

    if (stack.(sp-1) <> 1) then raise(Failure("Invalid array address.")) else 
    if (stack.(sp-3) <> 1) then raise(Failure("Type error: Array index must be an integer.")) else 
    let i = stack.(sp-2) in (* array address *)
    let obj_id = stack.(sp-5) 
    and loffset = stack.(sp-4)
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
           stack.(fp+i-1-2*loffset) <- stack.(sp-5); 
           stack.(fp+i-2-2*loffset) <- stack.(sp-6); 
           exec fp (sp) (pc+1)
      | 7 -> (* Arraystring *)
          for j=1 to 40 do
            stack.(fp+i-j-40*loffset) <- stack.(sp-j-4)
          done;
          exec fp (sp) (pc+1)
      | 8 -> (* ArrayBrick *)
          for j=1 to 13 do
            stack.(fp+i-j-13*loffset) <- stack.(sp-j-4)
          done;
          exec fp (sp) (pc+1)
      | 9 -> (* ArrayPlayer *)
          for j=1 to 11 do
            stack.(fp+i-j-11*loffset) <- stack.(sp-j-4)
          done;
          exec fp (sp) (pc+1)
      | 10 -> (* ArrayMap *)
          for j=1 to 7 do
            stack.(fp+i-j-7*loffset) <- stack.(sp-j-4)
          done;
          exec fp (sp) (pc+1)
      | _ -> raise(Failure("Type error: Attempt to store value into array of unknown type."))
    )
  | Jsr(-1) -> (* DrawPlayer *) 
      print_endline "You've just drawn something!" ; exec fp sp (pc+1)
  | Jsr(-2) -> (* Run *)
      print_endline "You've just started running your program!" ; exec fp sp (pc+1)
  | Jsr(-3) -> (* printint *)
      print_endline (string_of_int stack.(sp-2)) ; exec fp sp (pc+1)
  | Jsr(-4) -> (* printstring *)
      let var_type_id = stack.(sp-1) in
      if var_type_id <> 2 then raise (Failure("Type error: Unable to call printstring on nonstring."))
      else let strLen = stack.(sp-2) in
              let rec buildStr remaining str = if (remaining > 0) then
                  buildStr (remaining-1) ((Char.escaped (char_of_int (stack.(sp-remaining-2)))) ^ str) else str in
                  print_endline (buildStr strLen "");
                  exec fp sp (pc+1)          
  | Jsr(-5) -> (* dumpstack *)
      Array.iter print_endline (Array.map string_of_int stack); 
  | Jsr(-6) -> (* CallGenerator function of map on top of stack *)
        let i = stack.(sp-7) in
        stack.(sp)   <- pc + 1 ; exec fp (sp+1) i
  | Jsr i   -> stack.(sp)   <- pc + 1       ; exec fp (sp+1) i
  | Ent i   -> stack.(sp)   <- fp           ; exec sp (sp+i+1) (pc+1)
  | Rts i   -> 
    let new_fp = stack.(fp) and new_pc = stack.(fp-1) and base = fp-i-1 in 
    (
      let obj_id = stack.(sp-1) in
      match obj_id with
        (* Int *) 
        1 -> (stack.(base+1) <- stack.(sp-1);  (* Construct an int on top of stack*)
              stack.(base) <- stack.(sp-2);
              exec new_fp (base+2) new_pc)
      | 2 ->
            (for j=0 to 39 do
              stack.(base+j) <- stack.(sp-(40-j))
            done;
            exec new_fp (base+40) new_pc)
          
      
     (* | 3 -> let sp1 = stack.(sp-1) 
           and sp2 = stack.(sp-2) 
           and sp3 = stack.(sp-3) 
           and sp4 = stack.(sp-4) 
           and sp5 = stack.(sp-5) in
           (stack.(base+4) <- sp1;  (* Construct an int on top of stack*)
           stack.(base+3) <- sp2;
           stack.(base+2) <- sp3;
           stack.(base+1) <- sp4;
           stack.(base) <- sp5;
           exec new_fp (base+5) new_pc
           )
      | 4 -> let sp1 = stack.(sp-1) 
           and sp2 = stack.(sp-2) 
           and sp3 = stack.(sp-3) 
           and sp4 = stack.(sp-4) 
           and sp5 = stack.(sp-5) in
           (stack.(base+4) <- sp1;  (* Construct an int on top of stack*)
           stack.(base+3) <- sp2;
           stack.(base+2) <- sp3;
           stack.(base+1) <- sp4;
           stack.(base) <- sp5;
           exec new_fp (base+5) new_pc
           )
      | 5 -> let sp1 = stack.(sp-1) 
           and sp2 = stack.(sp-2) 
           and sp3 = stack.(sp-3) 
           and sp4 = stack.(sp-4) 
           and sp5 = stack.(sp-5) in
           (stack.(base+4) <- sp1;  (* Construct an int on top of stack*)
           stack.(base+3) <- sp2;
           stack.(base+2) <- sp3;
           stack.(base+1) <- sp4;
           stack.(base) <- sp5;
           exec new_fp (base+5) new_pc
           )*)
      | 6 -> (* Arrayint *)
            (for j=0 to 200 do
              stack.(base+j) <- stack.(sp-(201-j))
            done;
            exec new_fp (base+201) new_pc)

      | 7 -> (* Arraystring *)
            (for j=0 to 4000 do
              stack.(base+j) <- stack.(sp-(4001-j))
            done;
            exec new_fp (base+4001) new_pc)

      | 8 -> (* ArrayBrick *)
          (for j=0 to  1300 do
            stack.(base+j) <- stack.(sp-(1301-j))
          done;
          exec new_fp (base+1301) new_pc)

      | 9 -> (* ArrayPlayer *)
          (for j=0 to  1100 do
            stack.(base+j) <- stack.(sp-(1101-j))
          done;
          exec new_fp (base+1101) new_pc)

       | 10 ->  (* ArrayMap *)
            (for j=0 to  700 do
              stack.(base+j) <- stack.(sp-(701-j))
            done;
            exec new_fp (base+701) new_pc)

      | _ -> raise(Failure("Unmatched type in Rts!!"));
      );
  | Beq i   -> exec fp (sp-1) (pc + if stack.(sp-2) =  0 then i else 1)
  | Bne i   -> exec fp (sp-1) (pc + if stack.(sp-2) != 0 then i else 1)
  | Bra i   -> exec fp sp (pc+i)
  | Make id   -> 
    (match id with 
        1   -> raise(Failure("'Make' not required for int"));
      | 2   -> raise(Failure("'Make' not required for string"));
      | 3   -> exec fp (sp-1) (pc+1)
      | 4   -> exec fp (sp-1) (pc+1)
      | 5   -> exec fp (sp-1) (pc+1)
      | 6   -> stack.(sp+200)   <- id ; exec fp (sp+201) (pc+1)
      | 7   -> stack.(sp+4000)   <- id ; exec fp (sp+4001) (pc+1)
      | 8   -> stack.(sp+1300)   <- id ; exec fp (sp+1301) (pc+1)
      | 9   -> stack.(sp+1100)   <- id ; exec fp (sp+1101) (pc+1)
      | 10  -> stack.(sp+700)   <- id ; exec fp (sp+701) (pc+1)
      | _   -> raise(Failure("'Make' cannot apply to the invalid type " ^ string_of_int id));
    )
  (* Lodf and Strf *)
  | OpenWin -> (* Opens graphical display *) 
      Graphics.open_graph ""; exec fp (sp) (pc+1)
  | CloseWin -> (* Closes graphical display *)
      Graphics.clear_graph (); exec fp (sp) (pc+1)
  | CheckCollision -> (* Put a litint 1 or 0 on top of stack depending on whether there is a collision of player and bricks *)
        ()
  | CheckUserInput -> (* Change player on top of stack according to keyboard input *)
        ()
  | DrawPlayer -> (* Draws the player on top of the stack *)
        ()
  | PrintScore -> (* Prints the user's current score *)
        ()
  | Nt ->
    if (stack.(sp-1) <> 1) then 
      raise(Failure("Cannot apply 'Not' to non-int")) else
        if (stack.(sp-2) = 1) then stack.(sp-2) <- 0
        else if stack.(sp-2) = 0 then stack.(sp-2) <- 1
        else raise(Failure("'Not' can only apply to 1 or 0"));
      exec fp sp (pc+1)
  | Hlt     -> ()
with _ as error -> print_endline ("Execution error: " ^ (Printexc.to_string error) ^ " at PC " ^ (string_of_int pc) ^ ". Check the bytecode output with -b option.")
  in exec 0 0 0
