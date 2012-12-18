open Ast
open Bytecode

module StringMap = Map.Make(String)

let array_def_size = 100

(* Symbol table: Information about all the names in scope *)
type env = {
    function_index : int StringMap.t; (* Index for each function *)
    global_index   : int StringMap.t; (* "Address" for global variables *)
    local_index    : int StringMap.t; (* FP offset for args, locals *)
  }

(*
  Variable type map:
  int           : 1
  string        : 2
  Brick         : 3
  Player        : 4
  Map           : 5
  Arrayint      : 6
  Arraystring   : 7
  ArrayBrick    : 8
  ArrayPlayer   : 9
  ArrayMap      : 10
*)

let string_split s =
  let rec f str lst =
    try
      if (String.length str) = 0 then 
        lst 
      else
        let space_index = (String.index str ' ')
        and slength = (String.length str) in
        f (String.sub str (space_index + 1) (slength - space_index - 1)) 
          (if (space_index = 0) then lst else (String.sub str 0 space_index :: lst))
    with Not_found -> str :: lst
  in f s [];;

(* val enum : int -> 'a list -> (int * 'a) list *)
let rec enum stride n = function
    [] -> []
  | hd::tl -> 
    if stride > 0 then
      match hd.vartype with
        "int" ->    (n + 1, hd.varname) :: enum stride (n+stride * 2) tl
      | "string" -> (n + 39, hd.varname) :: enum stride (n+stride * 40) tl 
      | "Brick" ->
        (n,     hd.varname ^ ".$y" ) ::
        (n + 1, hd.varname ^ ".$x" ) ::
        (n + 2, hd.varname ^ ".$vertices") ::
        (n + 5, hd.varname ^ ".$color") ::
        (n + 6, hd.varname) :: enum stride (n+stride * 7) tl
          (* Brick size : 3 int (color), 1 int for vertex array, 2 for x and y, 1 int for type (3) = 7 *) 
      | "Player" -> 
        (n,     hd.varname ^ ".$y") ::
        (n + 1, hd.varname ^ ".$vertices") ::
        (n + 2, hd.varname ^ ".$color") ::
        (n + 5, hd.varname) :: enum stride (n+stride * 6) tl 
          (* Player size :  3 int (color), 1 int for vertex array, 1 int (y), 1 for type (4) = 6 *)
      | "Map" ->    
        (n,     hd.varname ^ ".$generator") ::
        (n + 1, hd.varname ^ ".$height") ::
        (n + 2, hd.varname ^ ".$width") ::
        (n + 3, hd.varname) :: enum stride (n+stride * 4) tl 
          (* Map size : 1 int for generator function, 2 x 1 int (h, w), 1 for type (5) = 4 *)
      | "Arrayint" ->    (n + 2*array_def_size-1, hd.varname) :: enum stride (n+stride * 2 * array_def_size + 1) tl
      | "Arraystring" -> (n + 40*array_def_size-1, hd.varname) :: enum stride (n+stride * 40 * array_def_size + 1) tl
      | "ArrayBrick" ->  (n + 7*array_def_size-1, hd.varname) :: enum stride (n+stride * 7 * array_def_size + 1) tl
      | "ArrayPlayer" -> (n + 6*array_def_size-1, hd.varname) :: enum stride (n+stride * 6 * array_def_size + 1) tl
      | "ArrayMap" ->    (n + 4*array_def_size-1, hd.varname) :: enum stride (n+stride * 4 * array_def_size + 1) tl
      | _ -> raise(Failure ("Undefined type with variable" ^ hd.varname))
    else
      match hd.vartype with
        "int" ->    (n, hd.varname) :: enum stride (n+stride * 2) tl
      | "string" -> (n, hd.varname) :: enum stride (n+stride * 40) tl 
      | "Brick" ->  
        (n - 6, hd.varname ^ ".$y" ) ::
        (n - 5, hd.varname ^ ".$x" ) ::
        (n - 4, hd.varname ^ ".$vertices") ::
        (n - 1, hd.varname ^ ".$color") ::
        (n, hd.varname) :: enum stride (n+stride * 7) tl 
      | "Player" -> 
        (n - 5, hd.varname ^ ".$y" ) ::
        (n - 4, hd.varname ^ ".$vertices") ::
        (n - 1, hd.varname ^ ".$color") ::
        (n, hd.varname) :: enum stride (n+stride * 6) tl 
      | "Map" ->    
        (n - 3, hd.varname ^ ".$generator" ) ::
        (n - 2, hd.varname ^ ".$height" ) ::
        (n - 1, hd.varname ^ ".$width") ::
        (n, hd.varname) :: enum stride (n+stride * 4) tl 
      | "Arrayint" ->    (n, hd.varname) :: enum stride (n+stride * 2 * array_def_size + 1) tl
      | "Arraystring" -> (n, hd.varname) :: enum stride (n+stride * 40 * array_def_size + 1) tl
      | "ArrayBrick" ->  (n, hd.varname) :: enum stride (n+stride * 7 * array_def_size + 1) tl
      | "ArrayPlayer" -> (n, hd.varname) :: enum stride (n+stride * 6 * array_def_size + 1) tl
      | "ArrayMap" ->    (n, hd.varname) :: enum stride (n+stride * 4 * array_def_size + 1) tl
      | _ -> raise(Failure ("Undefined type with variable" ^ hd.varname))

(* val enum : int -> 'a list -> (int * 'a) list *)
let rec enum_func stride n = function
    [] -> []
  | hd::tl -> (n, hd) :: enum_func stride (n+stride) tl

let total_varsize a vlist = 
   List.fold_left (fun a b -> a + (match b.vartype with
                    "int" -> 2  
                  | "string" -> 40
                  | "Brick" -> 7
                  | "Player" -> 6
                  | "Map" -> 4
                  | "Arrayint" -> array_def_size*2+1
                  | "Arraystring"  -> array_def_size*40+1
                  | "ArrayBrick" -> array_def_size*7+1
                  | "ArrayPlayer" -> array_def_size*6+1
                  | "ArrayMap" -> array_def_size*4+1
                  |  _ -> raise(Failure("Error in total_varsize"))
                  )) 0 vlist

(* val string_map_pairs StringMap 'a -> (int * 'a) list -> StringMap 'a *)
let string_map_pairs map pairs =
  List.fold_left (fun m (i, n) -> StringMap.add n i m) map pairs

(** Translate a program in AST form into a bytecode program.  Throw an
    exception if something is wrong, e.g., a reference to an unknown
    variable or function *)
let translate (globals, functions) =

  (* Allocate "addresses" for each global variable *)
  let global_indexes = string_map_pairs StringMap.empty (enum 1 0 globals) in

  (* Assign indexes to function names *)
  let built_in_functions = StringMap.add "draw" (-1) StringMap.empty in
  let built_in_functions = StringMap.add "run" (-2) built_in_functions in
  let built_in_functions = StringMap.add "printint" (-3) built_in_functions in
  let built_in_functions = StringMap.add "printstring" (-4) built_in_functions in
  let built_in_functions = StringMap.add "printarray" (-5) built_in_functions in
  let built_in_functions = StringMap.add "dumpstack" (-6) built_in_functions in
  let built_in_functions = StringMap.add "push" (-7) built_in_functions in

  let function_indexes = string_map_pairs built_in_functions
      (enum_func 1 1 (List.map (fun f -> f.fname) functions)) in

  (* Translate a function in AST form into a list of bytecode statements *)
  let translate env fdecl =
    (* Bookkeeping: FP offsets for locals and arguments *)
    let num_formals = total_varsize 0 fdecl.formals
    and num_locals = total_varsize 0 fdecl.locals
    and local_offsets = enum 1 1 fdecl.locals
    and formal_offsets = enum (-1) (-2) fdecl.formals in
    let env = { env with local_index = string_map_pairs
      StringMap.empty (local_offsets @ formal_offsets) } in

    let rec expr = function
        LiteralInt i -> [Litint i] 
      | LiteralString i -> [Litstr i]
      | Id s ->
          (try [Lfp (StringMap.find s env.local_index)]
           with Not_found -> try [Lod (StringMap.find s env.global_index)]
           with Not_found -> try [Lodf (StringMap.find s env.function_index)]
           with Not_found -> raise (Failure ("undeclared variable " ^ s)))

      | Brick (color, varray, x, y) ->
          expr y @ expr x 
          @ (try [Litint (StringMap.find varray env.local_index)]
             with Not_found -> try [Litint (StringMap.find varray env.global_index)]
             with Not_found -> raise (Failure ("undeclared variable " ^ varray)))
          @ [Litint 0] @ [Litint 0] @ [Litint 0]
          @ [Litint 3] @ [MakeB]

      | Player (color, varray, y) ->
          expr y @ (try [Lfp (StringMap.find varray env.local_index)]
                    with Not_found -> try [Lod (StringMap.find varray env.global_index)]
                    with Not_found -> raise (Failure ("undeclared variable " ^ varray)))
          @ [Litint 0] @ [Litint 0] @ [Litint 0]
          @ [Litint 4] @ [MakeP]

      | Map (width, height, generator) ->
          (try [Litint (StringMap.find generator env.function_index)]
           with Not_found -> raise (Failure ("undeclared function " ^ generator))) 
          @ expr height @ expr width @ [Litint 5] @ [MakeM]

      | Array (array_type) -> (* Push an empty array onto stack with type identifier on top *)
          let rec initializeEmptyArray size lst =
            if (size > 0) then initializeEmptyArray (size-1) ([Litint 0] @ lst)
            else lst in
            (
              match array_type with
                  "int" ->
                    (initializeEmptyArray 200 []) @ [Litint 6]
              |   "string" ->
                    (initializeEmptyArray 4000 []) @ [Litint 7]
              |   "Brick" ->
                    (initializeEmptyArray 700 []) @ [Litint 8]
              |   "Player" ->
                    (initializeEmptyArray 600 []) @ [Litint 9]
              |   "Map" -> 
                    (initializeEmptyArray 400 []) @ [Litint 10]
            )
     (* | Ref (base, child) ->
        let childTypeIndex = 
          (match child with
            "$vertices" -> 1
            | "$color" -> 2
            | "$x" -> 3
            | "$y" -> 4
            | "$height" -> 5
            | "$width" -> 6
            | "$generator" -> 7) in
          [Litint childTypeIndex]
          @ (try [Lfp (StringMap.find base env.local_index)]
           with Not_found -> try [Lod (StringMap.find base env.global_index)]
           with Not_found -> raise (Failure ("undeclared variable " ^ s))) @ [LodRef]
*)
      | AAccess(a, i) -> 
          expr i @ 
          (try [Lfpa (StringMap.find a env.local_index)]
          with Not_found -> try[Loda (StringMap.find a env.global_index)]
          with Not_found -> raise (Failure ("undeclared variable" ^ a)))
      | AAssign(a, i, e) ->
          expr e @ expr i @         
          (try [Sfpa (StringMap.find a env.local_index)]
          with Not_found -> try [Stra (StringMap.find a env.global_index)]
          with Not_found -> raise (Failure ("undeclared variable" ^ a)))
      | Binop (e1, op, e2) -> expr e1 @ expr e2 @ [Bin op]
  (*    | Not (e) -> 
        (match e with
          1 -> [Litint 0]
        | 0 -> [Litint 1]
        | _ -> raise (Failure ("'Not' cannot operate on" ^ e)))
      | Assign (s, e) ->
        (match s with
          Id(aStr) ->
            if String.find aStr "$.color" then 
              (match e with
                  LiteralString(eStr) -> 
                    let colors = Str.split(Str.regexp("[ \t]+"))(eStr) in
                      try let address = StringMap.find aStr env.local_index in
                        [Litint (int_of_string (List.nth colors 0))] @ [Sfp address] @
                        [Litint (int_of_string (List.nth colors 1))] @ [Sfp address-2] @
                        [Litint (int_of_string (List.nth colors 2))] @ [Sfp address-4]
                      with Not_found ->
                      try let address = StringMap.find aStr env.global_index in
                        [Litint (int_of_string (List.nth colors 0))] @ [Sfp address] @
                        [Litint (int_of_string (List.nth colors 1))] @ [Sfp address-2] @
                        [Litint (int_of_string (List.nth colors 2))] @ [Sfp address-4]
                  | Id()
              )
            else   
            try [Sfp (StringMap.find (expr s) env.local_index)]
            with Not_found -> try [Str (StringMap.find s env.global_index)]
            with Not_found -> raise (Failure ("undeclared variable " ^ s))
          | _ -> raise (Failure ("Assignment must be made to variable"))
        ) *)
          
      | Call (fname, actuals) ->
           (List.concat (List.map expr (List.rev actuals))) @
           (try [Jsr (StringMap.find fname env.function_index)]   
            with Not_found -> raise (Failure ("undefined function " ^ fname)))
      | Noexpr -> []

    in let rec stmt = function
        Block sl     ->  List.concat (List.map stmt sl)
      | Expr e       -> expr e @ [Drp]
      | Return e     -> expr e @ [Rts num_formals]
      | If (p, t, f) -> let t' = stmt t and f' = stmt f in
                         expr p @ [Beq(2 + List.length t')] @
                         t' @ [Bra(1 + List.length f')] @ f'
      | For (e1, e2, e3, b) -> stmt (Block([Expr(e1); While(e2, Block([b; Expr(e3)]))]))
      | While (e, b) -> let b' = stmt b and e' = expr e in
                         [Bra (1+ List.length b')] @ b' @ e' @
                         [Bne (-(List.length b' + List.length e'))]

    in [Ent num_locals] @           (* Entry: allocate space for locals *)
       stmt (Block fdecl.body) @    (* Body *)
       [Litint 0; Rts num_formals]  (* Default = return 0 *)

    in let env = { function_index = function_indexes;
                   global_index = global_indexes;
                   local_index = StringMap.empty } in

  (* Code executed to start the program: Jsr main; halt *)
    let entry_function = 
        try [OpenWin; Jsr (StringMap.find "$main" function_indexes); Hlt]
        with Not_found -> raise (Failure ("no \"$main\" function"))
    in
    
  (* Compile the functions *)
  let func_bodies = entry_function :: List.map (translate env) functions in

  (* Calculate function entry points by adding their lengths *)
  let (fun_offset_list, _) = List.fold_left
      (fun (l,i) f -> (i :: l, (i + List.length f))) ([],0) func_bodies in
  let func_offset = Array.of_list (List.rev fun_offset_list) in

  { num_globals = (total_varsize 0 globals);
    (* Concatenate the compiled functions and replace the function
       indexes in Jsr statements with PC values *)
    text = Array.of_list (List.map (function 
                                      Jsr i when i > 0 -> Jsr func_offset.(i)
                                    | _ as s -> s) (List.concat func_bodies))
  }
