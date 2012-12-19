open Ast
open Bytecode
open Thread

exception IllegalMove;;
exception End;;

type blockType = {
  mutable block_vertices:int list;
  mutable block_color:int;
};;

type playerType = {
  mutable player_vertices:int list;
  mutable player_color:int;
};;

type state = {
  mutable winWidth:int; 
  mutable winHeight:int; 
  mutable winBgColor:int;
  mutable reset: bool;
  mutable blockData:blockType list;
  mutable gravityFlag: int;
  mutable playerData:playerType;
  mutable userscore: int;
};;

let array_def_size = 100
(*let global_score = 0*)

let explode s =
  let rec f acc = function
    | -1 -> acc
    | k -> f (s.[k] :: acc) (k - 1)
  in f [] (String.length s - 1) ;;

let countArray stack globals sp = (* Count array on top of stack *)
  let rec countItems size t index count =
      if index = 100 then count else
        let itemtype = stack.(sp-2-size*index) in
        (if itemtype = 0 then (countItems size t (index+1) count) else 
          (if itemtype = t then (countItems size t (index+1) (count+1)) else 
            raise(Failure("Encountered array item of invalid type.")))) in
          (
            match stack.(sp-1) with
                6 -> (countItems 2 1 0 0) 
            |   7 -> (countItems 40 2 0 0)
            |   8 -> (countItems 13 3 0 0)
            |   9 -> (countItems 11 4 0 0)
            |   10 -> (countItems 7 5 0 0)
            |   _ -> raise(Failure("Type error: Array is of unknown type."))
          );;

let getNextFreeIndex stack globals sp isLocal =
  let rec countItems size count t =
      if count = 100 then count else
        (if (if (isLocal <> 1) then (globals.(sp-1-size*count)) else (stack.(sp-1-size*count))) <> t then count else
         countItems size (count+1) t) in
          (
            match (if (isLocal <> 1) then (globals.(sp)) else (stack.(sp))) with
                6 -> (countItems 2 0 1) 
            |   7 -> (countItems 40 0 2)
            |   8 -> (countItems 13 0 3)
            |   9 -> (countItems 11 0 4)
            |   10 -> (countItems 7 0 5)
            |   _ -> raise(Failure("Type error: Array is of unknown type."))
          );;


(********************************************
  Graphics helpers
*********************************************)
let draw_polygon vlist color =
  Graphics.set_color color;
  let x0 = (List.nth vlist 0) and y0 = (List.nth vlist 1) in
    (*print_endline( "x0, y0" ^ (string_of_int x0) ^ " " ^ (string_of_int y0) );*)
    Graphics.moveto x0 y0;
    for i = 1 to ((List.length vlist) / 2) - 1 do
      let x = (List.nth vlist (2*i)) and y = (List.nth vlist (2*i + 1)) in Graphics.lineto x y;
      (*print_endline( "x, y" ^ (string_of_int x) ^ " " ^ (string_of_int y) )*)
    done;
    Graphics.lineto x0 y0;
    (*print_endline( "x0, y0" ^ (string_of_int x0) ^ " " ^ (string_of_int y0) );
    print_endline("");*)

  let rec buildTupleArray = function
    [] -> []
    | px::py::tl -> (px,py)::(buildTupleArray tl)
    | _ :: [] -> raise(Failure("The vertices array provided does not contain a complete set of x,y coordinates.")) 
  in
  Graphics.fill_poly (Array.of_list (buildTupleArray vlist));;

(* Convert (r,g,b) into a single OCaml color value c *)
let color_from_rgb r g b =
  r*256*256 + g*256 + b;;

(*
  Relatively translate all vertex given the translation distance ex
*)
let rec trans_allVertices_x ex = function
  [] -> []
  | px::py::tl -> (px + ex)::(py::(trans_allVertices_x ex tl))
  | _ :: [] -> raise(Failure("The vertices array provided does not contain a complete set of x,y coordinates."));;

(*
  Relatively translate all vertex given the translation distance ey
*)
let rec trans_allVertices_y ey = function
  [] -> []
  | px::py::tl ->(px)::((py + ey)::(trans_allVertices_y ey tl))
  | _ :: [] -> raise(Failure("The vertices array provided does not contain a complete set of x,y coordinates."));;

(*
  Given absolute location in x of the first vertex of the polygon,
  rigidly translate all vertex relative to this absolute location
*)
let trans_allVertices_abs_x abx vlist =
  let distant = abx - (List.nth vlist 0) in
    let rec trans_abs_x dist = function
      [] -> []
      | px::py::tl -> (px + dist)::(py::(trans_abs_x dist tl))
      | _ :: [] -> raise(Failure("The vertices array provided does not contain a complete set of x,y coordinates.")) in
        trans_abs_x distant vlist;;

(*
  Given absolute location in y of the first vertex of the polygon,
  rigidly translate all vertex relative to this absolute location
*)
let trans_allVertices_abs_y aby vlist =
  let distant = aby - (List.nth vlist 1) in
    let rec trans_abs_y dist = function
      [] -> []
      | px::py::tl -> (px)::((py + dist)::(trans_abs_y dist tl))
      | _ :: [] -> raise(Failure("The vertices array provided does not contain a complete set of x,y coordinates.")) in
        trans_abs_y distant vlist;;

(* Given the start value and the list of vertices, compute max or min *)
let rec find_max_y current = function
    []           -> current
    | px::py::tl -> if (py > current) then (find_max_y py tl) else (find_max_y current tl)
    | _ :: [] -> raise(Failure("The vertices array provided does not contain a complete set of x,y coordinates."));;

let rec find_min_y current = function
    []           -> current
    | px::py::tl -> if (py < current) then (find_min_y py tl) else (find_min_y current tl)
    | _ :: [] -> raise(Failure("The vertices array provided does not contain a complete set of x,y coordinates."));;

let rec find_max_x current = function
    []           -> current
    | px::py::tl -> if (px > current) then (find_max_x px tl) else (find_max_x current tl)
    | _ :: [] -> raise(Failure("The vertices array provided does not contain a complete set of x,y coordinates."));;

let rec find_min_x current = function
    []           -> current
    | px::py::tl -> if (px < current) then (find_min_x px tl) else (find_min_x current tl)
    | _ :: [] -> raise(Failure("The vertices array provided does not contain a complete set of x,y coordinates."));;

(*
  Given a list of vertex coordinates [x0, y0, x1, y1, ...] and color,
  draw and fill the polygon. 
  *)
let draw_polygon vlist color =

  Graphics.set_color color;
  let x0 = (List.nth vlist 0) and y0 = (List.nth vlist 1) in
    Graphics.moveto x0 y0;
    for i = 1 to ((List.length vlist) / 2) - 1 do
      let x = (List.nth vlist (2*i)) and y = (List.nth vlist (2*i + 1)) in Graphics.lineto x y;
    done;
    Graphics.lineto x0 y0;

  let rec buildTupleArray = function
    [] -> []
    | px::py::tl -> (px,py)::(buildTupleArray tl)
    | _ :: [] -> raise(Failure("The vertices array provided does not contain a complete set of x,y coordinates.")) 
  in
  Graphics.fill_poly (Array.of_list (buildTupleArray vlist));;

(* Draw the moving block *)
let draw_rectangle x y size color =
  Graphics.set_color color;
  Graphics.fill_rect (x) (y) size size;;

let draw_string x y str =
  Graphics.moveto x y;
  Graphics.set_text_size 30;
  Graphics.draw_string str;;


(********************************************
  Execute the program
*********************************************)
let execute_prog prog =
  let stack = Array.make 32768 0
  and globals = Array.make prog.globals_size 0
  and random = Random.self_init ()

  and blocks = []
  (*and blocks2 = []*)
  and player = {player_vertices = []; player_color = 0}; in


  let rec exec fp sp pc = try match prog.text.(pc) with
    Litint i  -> 
    stack.(sp) <- i ; stack.(sp+1) <- 1; exec fp (sp+2) (pc+1) 
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
      (
        print_endline("Drop " ^ string_of_int var_type_id);
        match var_type_id with
          -1 -> exec fp (sp-2) (pc+1)
        | 1 -> exec fp (sp-2) (pc+1)
        | 2 -> exec fp (sp-40) (pc+1)
        | 3 -> exec fp (sp-13) (pc+1)
        | 4 -> exec fp (sp-11) (pc+1)
        | 5 -> exec fp (sp-7) (pc+1)
        | 6 -> exec fp (sp-array_def_size*2-1) (pc+1)
        | 7 -> exec fp (sp-array_def_size*40-1) (pc+1) 
        | 8 -> exec fp (sp-array_def_size*13-1) (pc+1)
        | 9 -> exec fp (sp-array_def_size*11-1) (pc+1)
        | 10 -> exec fp (sp-array_def_size*7-1) (pc+1)
        | _ -> raise(Failure("Unmatched type in Drp. Attempt to drop type " ^ string_of_int var_type_id)))
  | Bin op -> 
      let op1 = stack.(sp-4) 
      and op1type = stack.(sp-3)
      and op2 = stack.(sp-2)
      and op2type = stack.(sp-1) in
      if ((op1type <> op2type) || (op1type <> 1)) then raise(Failure("Binary operations can only be done on integers.")) else     
      stack.(sp-4) <- (let boolean i = if i then 1 else 0 in
      match op with
	      Add     -> op1 + op2
      | Sub     -> op1 - op2
      | Mult    -> op1 * op2
      | Div     -> op1 / op2
      | Mod     -> op1 mod op2
      | Equal   -> boolean (op1 = op2)
      | Neq     -> boolean (op1 != op2)
      | Less    -> boolean (op1 <  op2)
      | Leq     -> boolean (op1 <= op2)
      | Greater -> boolean (op1 >  op2)
      | Geq     -> boolean (op1 >= op2)
      | And     -> (if op1 == 0 || op2 == 0 then 0 else 1)
      | Or      -> (if op1 == 1 || op2 == 1 then 1 else 0));
      exec fp (sp-2) (pc+1)
  | Lod i -> (* Load a global variable *)

    let var_address = (if i = -32769 then
        (if (stack.(sp-1) <> 1) then raise(Failure("Attempt to access invalid address.")) else
        stack.(sp-2))
      else i) in
  
    let var_type_id = try globals.(i) 
                      with Invalid_argument ("i out of bounds") -> raise (Failure("Error: Attempt to load undeclared global variable."))
      in
        (match var_type_id with
            -1 -> (* negative 1 signifies local reference to follow *)
              stack.(sp) <- globals.(i-1);
              stack.(sp+1) <- globals.(i);
              exec fp (sp+2) (pc+1)
          | 1 -> (* int *)
              stack.(sp) <- globals.(i-1);
              stack.(sp+1) <- globals.(i);
              exec fp (sp+2) (pc+1)
          | 2 -> (* string *)
              for j=0 to 39 do
                stack.(sp+j) <- globals.(var_address-39+j)
              done;
              exec fp (sp+40) (pc+1)
          | 3 ->  (* Brick *)
              for j=0 to 12 do
                stack.(sp+j) <- globals.(var_address-12+j)
              done;
              exec fp (sp+13) (pc+1)
          | 4 ->  (* Player *)
              for j=0 to 10 do
                stack.(sp+j) <- globals.(var_address-10+j)
              done;
              exec fp (sp+11) (pc+1)
          | 5 ->  (* Map *)
              for j=0 to 6 do
                stack.(sp+j) <- globals.(var_address-6+j)
              done;
              exec fp (sp+7) (pc+1)
          | 6 ->  (* Arrayint *)
              for j=0 to 200 do
                stack.(sp+j) <- globals.(var_address-200+j)
              done;
              exec fp (sp+201) (pc+1)
          | 7 ->  (* Arraystring *)
              for j=0 to 4000 do
                stack.(sp+j) <- globals.(var_address-4000+j)
              done;
              exec fp (sp+4001) (pc+1)
          | 8 ->  (* ArrayBrick *)
              for j=0 to 1300 do
                stack.(sp+j) <- globals.(var_address-1300+j)
              done;
              exec fp (sp+1301) (pc+1)
          | 9 ->  (* ArrayPlayer *)
              for j=0 to 1100 do
                stack.(sp+j) <- globals.(var_address-100+j)
              done;
              exec fp (sp+1101) (pc+1)
          | 10 ->  (* ArrayMap *)
              for j=0 to 700 do
                stack.(sp+j) <- globals.(var_address-700+j)
              done;
              exec fp (sp+701) (pc+1)
          | _ -> raise(Failure("Type error: Attempt to load unknown type!"))
        ) 
  | Str i -> (* Store a global variable variable *)
    let globaltypeid = globals.(i)
    and var_type_id = stack.(sp-1) in
    if (globaltypeid <> var_type_id) then raise(Failure("Attempt to set global variable to mismatched type.")) else
    ( 
      match var_type_id with
        -1 -> (* int *)
          globals.(i-1) <- stack.(sp-2);
          globals.(i) <- stack.(sp-1);
          exec fp (sp) (pc+1)
      | 1 -> (* int *)
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
    if (stack.(sp-1) <> 1) then raise(Failure("Invalid array address.")) else
    if (stack.(sp-3) <> 1) then raise(Failure("Type error: Array index must be an integer!")) else
    let i = stack.(sp-2)
    and elem_index = stack.(sp-4) in
    let var_type_id = globals.(i) in
    let cnst_offset = 4 in
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
      let var_address = (if i = -32769 then
        (if (stack.(sp-1) <> 1) then 
          raise(Failure("Attempt to access invalid address.")) 
        else stack.(sp-2))
      else i) in
      let var_type_id = stack.(fp+var_address) in
      (
          match var_type_id with
          
            -1 -> (* negative int *)    
              stack.(sp) <- stack.(fp+var_address-1); (* value *)
              stack.(sp+1) <- stack.(fp+var_address); (* type *)
              exec fp (sp+2) (pc+1)
          | 1 -> (* int *)
              stack.(sp) <- stack.(fp+var_address-1); (* value *)
              stack.(sp+1) <- stack.(fp+var_address); (* type *)
              exec fp (sp+2) (pc+1)
          | 2 -> (* string *)
              for j=0 to 39 do
                stack.(sp+j) <- stack.(fp+var_address-39+j)
              done;
              exec fp (sp+40) (pc+1)
          | 3 -> (* Brick *)
              for j=0 to 12 do
                stack.(sp+j) <- stack.(fp+var_address-12+j)
              done;
              exec fp (sp+13) (pc+1)
          | 4 -> (* Player *)
              for j=0 to 10 do
                stack.(sp+j) <- stack.(fp+var_address-10+j);
              done;
              exec fp (sp+11) (pc+1)
          | 5 -> (* Map *)
              for j=0 to 6 do
                stack.(sp+j) <- stack.(fp+var_address-6+j)
              done;
              exec fp (sp+7) (pc+1)
          | 6 -> (* Arrayint *)
              for j=0 to 200 do
                stack.(sp+j) <- stack.(fp+var_address-200+j)
              done;
              exec fp (sp+201) (pc+1)
          | 7 -> (* Arrayƒstring *)
              for j=0 to 4000 do
                stack.(sp+j) <- stack.(fp+var_address-4000+j)
              done;
              exec fp (sp+4001) (pc+1)
          | 8 -> (* ArrayBrick *)
              for j=0 to 1300 do
                stack.(sp+j) <- stack.(fp+var_address-1300+j)
              done;
              exec fp (sp+1301) (pc+1)
          | 9 -> (* ArrayPlayer *)
              for j=0 to 1100 do
                stack.(sp+j) <- stack.(fp+var_address-1100+j)
              done;
              exec fp (sp+1101) (pc+1)
          | 10 -> (* ArrayMap *)
              for j=0 to 700 do
                stack.(sp+j) <- stack.(fp+var_address-700+j)
              done;
              exec fp (sp+701) (pc+1)  
          | 0 -> (* Uninitialized variable *)
              raise(Failure("Attempt to load uninitialized local variable."))  
          | _ -> raise(Failure("Type error: Attempt to load variable of unknown type.")))
      
  | Sfp i   ->
      let localvartypeid = stack.(fp+i)
      and obj_id = stack.(sp-1) in
      if (obj_id <> localvartypeid) then raise(Failure("Attempt to store mismatched variable type in local variable.")) else
      ( 
        match obj_id with
          -1 -> (* int *)
            stack.(fp+i) <- stack.(sp-1); 
            stack.(fp+i-1) <- stack.(sp-2); 
            exec fp (sp) (pc+1)
        | 1 -> (* int *)
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
        | _ -> raise(Failure("Type error: Unmatched type error!"))
      )
  | Lfpa -> (* Load index of local array, based on next integer on stack *)
    if (stack.(sp-1) <> 1) then raise(Failure("Invalid array address.")) else
    if (stack.(sp-3) <> 1) then raise(Failure("Type error: Array index must be an integer.")) else
    let i = stack.(sp-2) in (* array address *)
    let cnst_offset = 4 in
    let obj_id = stack.(fp+i)
    and loffset = stack.(sp-4) in
    ( 
      match obj_id with
        6 -> (* Arrayint *)
          if stack.(fp+i-1-loffset*2) = 0 then raise(Failure("Attempt to load from array at an uninitialized index.")) else
          (stack.(sp-4) <- stack.(fp+i-2-loffset*2); (* value *)
          stack.(sp+1-4) <- stack.(fp+i-1-loffset*2); (* type *)
          exec fp (sp-2) (pc+1))
      | 7 -> (* Arraystring *)
          if stack.(fp+i-1-loffset*40) = 0 then raise(Failure("Attempt to load from array at an uninitialized index.")) else
          (for j=0 to 39 do
            stack.(sp+j-cnst_offset) <- stack.(fp+i-40+j-loffset*40)
          done;
          exec fp (sp+40-cnst_offset) (pc+1))
      | 8 -> (* ArrayBrick *)
          if stack.(fp+i-1-loffset*13) = 0 then raise(Failure("Attempt to load from array at an uninitialized index.")) else
          (for j=0 to 12 do
            stack.(sp+j-cnst_offset) <- stack.(fp+i-13+j-loffset*13)
          done;
          exec fp (sp+13-cnst_offset) (pc+1))
      | 9 -> (* ArrayPlayer *)
          if stack.(fp+i-1-loffset*11) = 0 then raise(Failure("Attempt to load from array at an uninitialized index.")) else
          (for j=0 to 10 do
            stack.(sp+j-cnst_offset) <- stack.(fp+i-11+j-loffset*11);
          done;
          exec fp (sp+11-cnst_offset) (pc+1))
      | 10 -> (* ArrayMap *)
          if stack.(fp+i-1-loffset*7) = 0 then raise(Failure("Attempt to load from array at an uninitialized index.")) else
          (for j=0 to 6 do
            stack.(sp+j-cnst_offset) <- stack.(fp+i-7+j-loffset*7)
          done;
          exec fp (sp+7-cnst_offset) (pc+1))
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
  
  | Lref ->
    if (stack.(sp-1) = 1) then (* GLOBAL ADDRESS *)
    (
      if (stack.(sp-3) <> 1) then raise(Failure("Type error: Array index must be an integer!")) else
      let i = stack.(sp-2)
      and elem_index = stack.(sp-4) in
      let var_type_id = globals.(i) in
      let cnst_offset = 4 in
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
      if (globals.(i-1-elem_size*elem_index) = 0) then raise(Failure("Attempt to access an index of an array that has not been initialized yet.")) else
      (match var_type_id with
          6 -> (* Arrayint *)
            stack.(sp-4) <- globals.(i-2-elem_size*elem_index);
            stack.(sp+1-4) <- globals.(i-1-elem_size*elem_index);
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
    ) 
    else if (stack.(sp-1) = -1) then (*LOCAL ADDRESS*)
    (
      if (stack.(sp-3) <> 1) then raise(Failure("Type error: Array index must be an integer.")) else
      let i = stack.(sp-2) in (* array address *)
      let cnst_offset = 4 in
      let obj_id = stack.(fp+i)
      and loffset = stack.(sp-4) in
      let elem_size = (
        match obj_id with
            6 -> 2
        |   7 -> 40
        |   8 -> 13
        |   9 -> 11
        |   10 -> 7
        |   0 -> raise(Failure("Attempt to load an index from an uninitialized array."))
        |   _ -> raise(Failure("Unable to load an index from non array type."))
      ) in
      if (stack.(fp+i-1-loffset*elem_size) = 0) then raise(Failure("Attempt to access an index of an array that has not been initialized yet.")) else
      ( 
        match obj_id with
          6 -> (* Arrayint *)
            stack.(sp-4) <- stack.(fp+i-2-loffset*2); (* value *)
            stack.(sp-3) <- stack.(fp+i-1-loffset*2); (* type *)
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
    ) else raise(Failure("Invalid array address type. Must be int."))

  | Sref ->
      if (stack.(sp-1) = 1) then
      ( 
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
      ) 
      else if (stack.(sp-1) = -1) then
      (
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
      ) else raise(Failure("Invalid array address type. Must be int."))

  | Jsr(-1) -> (* DrawPlayer *) 
      print_endline ("You've just drawn something!") ;
      let scope = stack.(sp-8)
      and addr = stack.(sp-9) 
      and color = color_from_rgb stack.(sp-3) stack.(sp-5) stack.(sp-7)
      in
        let rec make_coord_list n = 
          if (scope = -1) then (*LOCAL*)
            (match stack.(fp+n) with
                0 -> []
              | 1 -> stack.(fp+n-1) :: make_coord_list (n-2)
              | _ -> raise(Failure("cant resolve " ^ string_of_int stack.(fp+n))))
          else if (scope = 1) then (*GLOBAL*)
            (match globals.(n) with
              0 -> []
            | 1 -> globals.(n-1) :: make_coord_list (n-2)
            | _ -> raise(Failure("cant resolve " ^ string_of_int globals.(n))))
          else [] in
        
        player = {player_vertices= make_coord_list (addr-1);player_color = color};
        exec fp sp (pc+1)
  | Jsr(-2) -> (* Run *)

(******************************************************************************************************************)
(******************************************************************************************************************)
(******************************************************************************************************************)

(* s is state *)
let t_init s () =
  Graphics.open_graph (" " ^ (string_of_int s.winWidth) ^ "x" ^
                             (string_of_int s.winHeight));
  Graphics.set_color s.winBgColor;
  Graphics.fill_rect 0 0 s.winWidth s.winHeight;
  (*Graphics.set_color s.player_color;*)
  draw_polygon s.playerData.player_vertices s.playerData.player_color;
  (*Graphics.set_color s.block1_color;*)
  
  List.iter (fun block -> (draw_polygon block.block_vertices
                                          block.block_color)) s.blockData;

  (*draw_rectangle s.block1_x s.block1_y s.block1_size s.block1_color;*)
in

(* s is state *)
let t_end s () =
  Graphics.close_graph ();
  Graphics.set_color s.winBgColor;
in

(* c is keyboad input (char) *)
let t_key s c =
  (*draw_player s.player_x s.player_y s.player_size s.player_color;*)
  
  
  let max_y = find_max_y 0 s.playerData.player_vertices 
  and min_y = find_min_y s.winHeight s.playerData.player_vertices in
  (*let objectheight = (max_y - (List.nth s.playerData.player_vertices 1)) in*)
  let objectheight = (max_y - min_y) in

    (match c with
      ' '   -> if max_y < s.winHeight then 
               (
                  if (s.gravityFlag < 2) then
                      s.gravityFlag <- 2;
                        
                  s.playerData.player_vertices <- (trans_allVertices_y s.gravityFlag s.playerData.player_vertices);
                  s.gravityFlag <- (s.gravityFlag + 3);
               )
               else
                  s.playerData.player_vertices <- 
                  (trans_allVertices_abs_y (s.winHeight - objectheight) s.playerData.player_vertices)
      (*|'z'   -> if min_y > 0 then 
                  s.playerData.player_vertices <- 
                  (trans_allVertices_y (-15) s.playerData.player_vertices)
                else
                  s.playerData.player_vertices <- 
                  (trans_allVertices_abs_y 0 s.playerData.player_vertices)*)
      | _     -> ());
in


let t_updateFrame s () =
  Graphics.clear_graph ();
  Graphics.set_color s.winBgColor;
  Graphics.fill_rect 0 0 s.winWidth s.winHeight;
  
  (*
  s.block1_x <- s.block1_x - 3;
  draw_rectangle s.block1_x s.block1_y s.block1_size s.block1_color;
  *)

    (*
    let rec trans_allVertices = function
      [] -> []
      | px::py::tl -> (px - 3)::(py::(trans_allVertices tl))
    in*)
  List.iter (fun block -> ( block.block_vertices <- 
                      (trans_allVertices_x (-10) block.block_vertices))) s.blockData;

  List.iter (fun block -> (draw_polygon block.block_vertices
                                          block.block_color)) s.blockData;
    
  (* Gravitify *)
  s.gravityFlag <- (s.gravityFlag - 1);
  let max_y = find_max_y 0 s.playerData.player_vertices 
  and min_y = find_min_y s.winHeight s.playerData.player_vertices in
  let objectheight = (max_y - (List.nth s.playerData.player_vertices 1)) in
      if (max_y > s.winHeight) then
             s.playerData.player_vertices <- 
                  (trans_allVertices_abs_y (s.winHeight - objectheight) s.playerData.player_vertices)
      else
        if (min_y > 0) then
          s.playerData.player_vertices <- (trans_allVertices_y s.gravityFlag s.playerData.player_vertices)
        else 
          s.playerData.player_vertices <- 
                  (trans_allVertices_abs_y 0 s.playerData.player_vertices);
  (* End Gravitify *)


  (* Wrap map *)
  s.reset <- true;
  let rec wrapAround = function
      [] -> []
      | px::py::tl -> if (px > 0) then s.reset <- false; (px)::(py::(wrapAround tl))
  in
  List.iter (fun block -> ( block.block_vertices <- (wrapAround block.block_vertices))) s.blockData;
  if (s.reset = true) then
    List.iter (fun block -> ( block.block_vertices <- (trans_allVertices_x (s.winWidth+s.winWidth) block.block_vertices))) s.blockData;

  (* End wrap map *)

  s.userscore <- s.userscore + 1;
  draw_polygon s.playerData.player_vertices s.playerData.player_color;
in
  

let t_except s ex = ();
in

let t_playerCollided s () =

  (* Get blockType block and return a GPC polygon *)
  let makeGPCPolygon vlist =
   let rec makeVertexArray = function
      []           -> [||]
      | px::py::tl -> Array.append [|{Clip.x = (float_of_int px); Clip.y = (float_of_int py)}|] (makeVertexArray tl) in
        Clip.make_gpcpolygon [|false|] [|(makeVertexArray vlist)|] in

  let checkCollision block =

      let _result = Clip.gpcml_clippolygon 
                      Clip.Intersection 
                      (makeGPCPolygon s.playerData.player_vertices) 
                      (makeGPCPolygon block.block_vertices) 
                    in
                      (Clip.gpcml_isOverlapped _result)
      in

        let result list = List.fold_left (fun a b -> a || b) false list in
          let collisionList = List.map checkCollision s.blockData in
            (*print_endline (string_of_bool (result collisionList));*)
            result collisionList;
in

(*let i = ref 0; in*)

let skel f_init f_end f_key f_updateFrame f_except f_playerCollided = 
  f_init ();
  try 
      while not (f_playerCollided ()) do
        try 

          if Graphics.key_pressed () then f_key (Graphics.read_key ());
          (*if f_playerCollided () then f_end ();*)
          Thread.join(Thread.create(Thread.delay)(1.0 /. 24.0));
          f_updateFrame ();
        with 
             End -> raise End
           |  e  -> f_except e
      done
  with 
      End  -> f_end ();

in
(*
let block1 = { block_vertices=
                [500; 200;
                650; 200;
                650; 350;
                500; 350;
                400; 350];
               block_color=(color_from_rgb 20 20 20) }; in
let block2 = { block_vertices=
                [600; 500;
                700; 450;
                700; 500;
                700; 600;
                600; 600];
               block_color=(color_from_rgb 150 20 120) }; in
let block3 = { block_vertices=
                [400; 0;
                450; 0;
                450; 50;
                400; 50];
               block_color=(color_from_rgb 20 120 20) }; in

let block4 = { block_vertices=
                [800; 0;
                850; 0;
                850; 50;
                800; 50];
               block_color=(color_from_rgb 20 120 20) }; in

let block5 = { block_vertices=
                [800; 40;
                850; 40;
                850; 70;
                800; 70];
               block_color=(color_from_rgb 20 120 20) }; in

let block6 = { block_vertices=
                [700; 200;
                750; 200;
                750; 250;
                700; 250];
               block_color=(color_from_rgb 20 120 20) }; in

let block7 = { block_vertices=
                [1000; 40;
                1050; 40;
                1050; 70;
                1000; 70];
               block_color=(color_from_rgb 20 120 20) }; in

let block8 = { block_vertices=
                [1100; 200;
                1150; 200;
                1150; 250;
                1100; 250];
               block_color=(color_from_rgb 20 120 20) }; in



let blocks = [block1; block2; block3; block4; block5; block6; block7; block8];
in


let player = { player_vertices=
                [50; 300;
                100; 300;
                100; 350;
                50; 350;
                25; 360];
               player_color=(color_from_rgb 20 120 20) }; 
in
*)
let gameState = {winWidth=800; winHeight=600; 
                winBgColor=(color_from_rgb 255 255 255);
                blockData=blocks;
                reset=true;
                gravityFlag=0;
                playerData=player;
                userscore=2};
in


let slate () =
    skel (t_init gameState) (t_end gameState)
         (t_key gameState) (t_updateFrame gameState) 
         (t_except gameState) (t_playerCollided gameState); 
in

slate ();
print_endline("Game End!");














      print_endline "You've just started running your program!" ; exec fp sp (pc+1)
  | Jsr(-3) -> (* printint *)
      if (stack.(sp-1) <> 1) then raise(Failure("The function $printint must take an integer value.")) else
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
        print_endline ("Call generator" ^  " " ^ string_of_int (i+fp)) ;
        stack.(sp)   <- pc + 1 ; exec fp (sp+1) i
  | Jsr(-7) -> (* Push function to push object on top of stack into array *)
      let varsize = (match (stack.(sp-1)) with
                        1 -> 2
                     |  2 -> 40
                     |  3 -> 13
                     |  4 -> 11
                     |  5 -> 7
                     |  _ -> raise(Failure("Unable to push object of unknown type onto array."))
                    ) in
        let isLocal = (stack.(sp-4-varsize)) in
        let nextIndex = (getNextFreeIndex stack globals (if isLocal <> 1 then (stack.(sp-2-varsize)) else (fp+(stack.(sp-2-varsize)))) isLocal) in
          (if nextIndex > 100 then raise(Failure("Unable to push value onto full array.")) else
            stack.(sp) <- nextIndex; stack.(sp+1) <- 1; exec fp (sp+2) (pc+1)
          ) 
  | Jsr(-8) -> (* GetCurrentScore function to put current score on stack *)
      (*stack.(sp) <- gameState.userscore; stack.(sp+1) <- 1; exec fp (sp+2) (pc+1)*)
      stack.(sp) <- 1; stack.(sp+1) <- 1; exec fp (sp+2) (pc+1)
  | Jsr(-9) -> (* GenerateRandomInt function to generate a random integer and put it on top of stack *)
      let seedtype = stack.(sp-1)
      and seed = stack.(sp-2) in
      if (seedtype <> 1) then raise(Failure("Type error: The function $GenerateRandomInt requires an integer parameter.")) else
      let generated = (Random.int seed) in
      stack.(sp) <- generated; stack.(sp+1) <- 1; exec fp (sp+2) (pc+1)
  | Jsr(-10) -> (* ArrayCount function to put number of elements in array on stack *)
        let arraycount = (countArray stack globals sp)
        and arrayType = stack.(sp-1) in
        if (arrayType < 6 || arrayType > 10) then raise(Failure("Unable to count the elements of a nonarray object.")) else
        (stack.(sp) <- arraycount; stack.(sp+1) <- 1; exec fp (sp+2) (pc+1))
  | Jsr i   -> stack.(sp)   <- pc + 1       ; exec fp (sp+1) i
  | Ent i   -> stack.(sp)   <- fp           ; exec sp (sp+i+1) (pc+1)
  | Rts i   -> 
    let new_fp = stack.(fp) and new_pc = stack.(fp-1) and base = fp-i-1 in 
    (
      print_endline(string_of_int new_fp ^ " " ^ string_of_int new_pc ^ " " ^ string_of_int base);
      let obj_id = stack.(sp-1) in
      match obj_id with
        1 -> (* int *) 
              (stack.(base+1) <- stack.(sp-1);  (* Construct an int on top of stack*)
              stack.(base) <- stack.(sp-2);
              exec new_fp (base+2) new_pc)

      | 2 -> (* string *)
            (for j=0 to 39 do
              stack.(base+j) <- stack.(sp-(40-j))
            done;
            exec new_fp (base+40) new_pc)
          
      | 3 -> (* Brick *)
            (for j=0 to 11 do
              stack.(base+j) <- stack.(sp-(12-j))
            done;
            exec new_fp (base+12) new_pc)

      | 4 -> (* Player *)
            (for j=0 to 9 do
              stack.(base+j) <- stack.(sp-(10-j))
            done;
            exec new_fp (base+10) new_pc)

      | 5 -> (* Map *)
            (for j=0 to 5 do
              stack.(base+j) <- stack.(sp-(6-j))
            done;
            exec new_fp (base+6) new_pc)

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
  | Bne i   -> (print_endline ("testcol" ^ (string_of_int (pc + if stack.(sp-2) != 0 then i else 1)))); exec fp (sp-2) (pc + if stack.(sp-2) != 0 then i else 1)
  | Bra i   -> exec fp sp (pc+i)
  | Make id   -> 
    (match id with 
        0   -> exec fp (sp-1) (pc+1)
      | 1   -> raise(Failure("'Make' not required for int"));
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
  | Init (i, j, k) -> 
      if (k <> 1) then
        (globals.(j) <- i; exec fp sp (pc+1))
      else
        (stack.(fp+j) <- i; exec fp sp (pc+1))
  (* Lodf and Strf *)
  | OpenWin -> (* Opens graphical display *)
      (*gameState.winHeight <- stack.(sp-3); gameState.winWidth <- stack.(sp-5);
      Graphics.open_graph (" "^(string_of_int gameState.winWidth)^"x"^(string_of_int gameState.winHeight));
      Graphics.set_color gameState.winBgColor;
      Graphics.fill_rect 0 0 gameState.winWidth gameState.winHeight;
      Thread.join(Thread.create(Thread.delay)(240.0 /. 240.0)); *)
      exec fp (sp) (pc+1)
  | CloseWin -> (* Closes graphical display *)
      Graphics.clear_graph (); exec fp (sp) (pc+1)
  | StoreWindow ->
      (*gameState.winWidth = stack.(sp-3); gameState.winHeight <- stack.(sp-5);*) exec fp (sp) (pc+1) 
  | CheckCollision -> (* Put a litint 1 or 0 on top of stack depending on whether there is a collision of player and bricks *)
        print_endline ("checking collision");

(*
      (* Scene is a list of blocks *)
      let t_playerCollided scene =
        let makeGPCPolygon vlist =
          let rec makeVertexArray = function
            []           -> [||]
            | px::py::tl -> Array.append [|{Clip.x = (float_of_int px); Clip.y = (float_of_int py)}|] (makeVertexArray tl) 
            | _          -> raise(Failure("Vertex array must be even-sized")) in
               Clip.make_gpcpolygon [|false|] [|(makeVertexArray vlist)|] in

        let checkPBCollision block =
          let _result = Clip.gpcml_clippolygon 
                      Clip.Intersection 
                      (makeGPCPolygon player.player_vertices) 
                      (makeGPCPolygon block.block_vertices) 
                      in
                      (Clip.gpcml_isOverlapped _result)
          in

          let result list = List.fold_left (fun a b -> a || b) false list in
            let collisionList = List.map checkPBCollision scene in
              (*print_endline (string_of_bool (result collisionList));*)
              result collisionList;
      in

      stack.(sp) <- (int_of_bool (t_playerCollided blocks1 || t_playerCollided blocks2) );
      stack.(sp+1) <- 1; 
      exec fp (sp+2) (pc+1)
      let final_result = (int_of_bool (t_playerCollided blocks1 || t_playerCollided blocks2) ) in*)
      stack.(sp-11) <- 1; stack.(sp-10) <- 1; exec fp (sp-9) (pc+1)

  | CheckUserInput -> (* Change player on top of stack according to keyboard input *)
      (*
      let max_y = find_max_y 0 player.player_vertices 
      and min_y = find_min_y gameState.winHeight player.player_vertices in
      let objectheight = (max_y - min_y) in

      if Graphics.key_pressed () then
      (match Graphics.read_key() with
      ' '   -> if max_y < gameState.winHeight then 
               (
                  if (gameState.gravityFlag < 2) then
                      gameState.gravityFlag <- 2;
                        
                  player.player_vertices <- (trans_allVertices_y gameState.gravityFlag player.player_vertices);
                  gameState.gravityFlag <- (gameState.gravityFlag + 3);
               )
               else
                  player.player_vertices <- 
                  (trans_allVertices_abs_y (gameState.winHeight - objectheight) player.player_vertices)

      | _     -> ());
       
*)


      exec fp sp (pc+1)
  | DrawPlayer -> (* Draws the player on top of the stack *)
     ()


  | ProcessBlocks -> (*Blocks are on the top of the stack*)
      let rec addToBricks i = 

        if (stack.(i-1) = 3) then
          (
          let scope = stack.(i-8)
          and r = stack.(i-3)
          and g = stack.(i-5)
          and b = stack.(i-7)
          and addr = stack.(i-9) in    
          let rec make_coord_list n = 
            if (scope = -1) then (*LOCAL*)
              (

                (*print_endline(string_of_int fp ^ " ! " ^ string_of_int stack.(fp+n-4)
                ^ " ! " ^ string_of_int stack.(fp+n-3)^ " ! " ^ string_of_int stack.(fp+n-2)
              ^ " ! " ^ string_of_int stack.(fp+n)^ " ! " ^ string_of_int stack.(fp+n-1));*)
                match stack.(fp+n) with
                  0 -> []
                | 1 -> stack.(fp+n-1) :: make_coord_list (n-2)
                | _ -> raise(Failure("cant resolve " ^ string_of_int stack.(fp+n))))
            else if (scope = 1) then (*GLOBAL*)
              (match globals.(n) with
                0 -> []
              | 1 -> globals.(n-1) :: make_coord_list (n-2)
              | _ -> raise(Failure("cant resolve " ^ string_of_int globals.(n))))
            else [] in
            (
            {block_vertices= make_coord_list (addr-1); block_color=r*256*256+g*256+b;} :: addToBricks (i-13)
            )
          ) else []
      in 
      blocks = addToBricks (sp-1);    

      (*print_endline (String.concat " " (List.map string_of_int ((List.hd blocks1).block_vertices)));

        
      draw_polygon ((List.hd blocks1).block_vertices) ((List.hd blocks1).block_color);

      Thread.join(Thread.create(Thread.delay)(3.0)); *)
      exec fp sp (pc+1)

  | PrintScore -> (* Prints the user's current score *)

(*
      print_endline("Score: " ^ string_of_int gameState.userscore);
      draw_string 0 stack.(sp-4) (string_of_int gameState.userscore);

      gameState.userscore = gameState.userscore + 1;
*)
      exec fp sp (pc+1)

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
