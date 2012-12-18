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
        (n + 1, hd.varname ^ ".$y" ) ::
        (n + 3, hd.varname ^ ".$x" ) ::
        (n + 5, hd.varname ^ ".$vertices") ::
        (n + 7, hd.varname ^ ".$colorB") ::
        (n + 9, hd.varname ^ ".$colorG") ::
        (n + 11, hd.varname ^ ".$colorR") ::
        (n + 12, hd.varname) :: enum stride (n+stride * 13) tl
          (* Brick size : 3 * 2 int (color), 1 * 2int for vertex array, 2 * 2 for x and y, 1 int for type (3) = 13 *) 
      | "Player" -> 
        (n + 1,     hd.varname ^ ".$y") ::
        (n + 3, hd.varname ^ ".$vertices") ::
        (n + 5, hd.varname ^ ".$colorB") ::
        (n + 7, hd.varname ^ ".$colorG") ::
        (n + 9, hd.varname ^ ".$colorR") ::
        (n + 10, hd.varname) :: enum stride (n+stride * 11) tl 
          (* Player size :  3 * 2 int (color), 1 * 2 int for vertex array, 1 * 2 int (y), 1 for type (4) = 11 *)
      | "Map" ->    
        (n + 1,     hd.varname ^ ".$generator") ::
        (n + 3, hd.varname ^ ".$height") ::
        (n + 4, hd.varname ^ ".$width") ::
        (n + 6, hd.varname) :: enum stride (n+stride * 7) tl 
          (* Map size : 1 * 2 int for generator function, 2 x 2 int (h, w), 1 for type (5) = 7 *)
      | "Arrayint" ->    (n + 2*array_def_size-1, hd.varname) :: enum stride (n+stride * 2 * array_def_size + 1) tl
      | "Arraystring" -> (n + 40*array_def_size-1, hd.varname) :: enum stride (n+stride * 40 * array_def_size + 1) tl
      | "ArrayBrick" ->  (n + 13*array_def_size-1, hd.varname) :: enum stride (n+stride * 13 * array_def_size + 1) tl
      | "ArrayPlayer" -> (n + 11*array_def_size-1, hd.varname) :: enum stride (n+stride * 11 * array_def_size + 1) tl
      | "ArrayMap" ->    (n + 7*array_def_size-1, hd.varname) :: enum stride (n+stride * 7 * array_def_size + 1) tl
      | _ -> raise(Failure ("Undefined type with variable" ^ hd.varname))
    else
      match hd.vartype with
        "int" ->    (n, hd.varname) :: enum stride (n+stride * 2) tl
      | "string" -> (n, hd.varname) :: enum stride (n+stride * 40) tl 
      | "Brick" ->  
        (n - 11, hd.varname ^ ".$y" ) ::
        (n - 9, hd.varname ^ ".$x" ) ::
        (n - 7, hd.varname ^ ".$vertices") ::
        (n - 5, hd.varname ^ ".$colorB") ::
        (n - 3, hd.varname ^ ".$colorG") ::
        (n - 1, hd.varname ^ ".$colorR") ::
        (n, hd.varname) :: enum stride (n+stride * 13) tl 
      | "Player" -> 
        (n - 9, hd.varname ^ ".$y" ) ::
        (n - 7, hd.varname ^ ".$vertices") ::
        (n - 5, hd.varname ^ ".$colorB") ::
        (n - 3, hd.varname ^ ".$colorG") ::
        (n - 1, hd.varname ^ ".$colorR") ::
        (n, hd.varname) :: enum stride (n+stride * 11) tl 
      | "Map" ->    
        (n - 5, hd.varname ^ ".$generator" ) ::
        (n - 3, hd.varname ^ ".$width" ) ::
        (n - 1, hd.varname ^ ".$height") ::
        (n, hd.varname) :: enum stride (n+stride * 7) tl 
      | "Arrayint" ->    (n, hd.varname) :: enum stride (n+stride * 2 * array_def_size + 1) tl
      | "Arraystring" -> (n, hd.varname) :: enum stride (n+stride * 40 * array_def_size + 1) tl
      | "ArrayBrick" ->  (n, hd.varname) :: enum stride (n+stride * 13 * array_def_size + 1) tl
      | "ArrayPlayer" -> (n, hd.varname) :: enum stride (n+stride * 11 * array_def_size + 1) tl
      | "ArrayMap" ->    (n, hd.varname) :: enum stride (n+stride * 7 * array_def_size + 1) tl
      | _ -> raise(Failure ("Undefined type with variable" ^ hd.varname))

(* val enum : int -> 'a list -> (int * 'a) list *)
let rec enum_func stride n = function
    [] -> []
  | hd::tl -> (n, hd) :: enum_func stride (n+stride) tl

let total_varsize a vlist = 
   List.fold_left (fun a b -> a + (match b.vartype with
                    "int" -> 2  
                  | "string" -> 40
                  | "Brick" -> 13
                  | "Player" -> 11
                  | "Map" -> 7
                  | "Arrayint" -> array_def_size*2+1
                  | "Arraystring"  -> array_def_size*40+1
                  | "ArrayBrick" -> array_def_size*13+1
                  | "ArrayPlayer" -> array_def_size*11+1
                  | "ArrayMap" -> array_def_size*7+1
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
  let built_in_functions = StringMap.add "DrawPlayer" (-1) StringMap.empty in
  let built_in_functions = StringMap.add "Run" (-2) built_in_functions in
  let built_in_functions = StringMap.add "printint" (-3) built_in_functions in
  let built_in_functions = StringMap.add "printstring" (-4) built_in_functions in
  let built_in_functions = StringMap.add "dumpstack" (-5) built_in_functions in
  let built_in_functions = StringMap.add "Push" (-6) built_in_functions in
  let built_in_functions = StringMap.add "CallGenerator" (-7) built_in_functions in

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
           (*with Not_found -> try [Lodf (StringMap.find s env.function_index)]*)
           with Not_found -> raise (Failure ("undeclared variable " ^ s)))

      | Brick (r, g, b, varray, x, y) ->
          expr y @ expr x 
          @ (try [Litint (StringMap.find varray env.local_index)]
             with Not_found -> try [Litint (StringMap.find varray env.global_index)]
             with Not_found -> raise (Failure ("undeclared variable " ^ varray)))
          @ expr b @ expr g @ expr r
          @ [Litint 3]

      | Player (r, g, b, varray, y) ->
          expr y @ (try [Lfp (StringMap.find varray env.local_index)]
                    with Not_found -> try [Lod (StringMap.find varray env.global_index)]
                    with Not_found -> raise (Failure ("undeclared variable " ^ varray)))
          @ expr b @ expr g @ expr r
          @ [Litint 4]

      | Map (width, height, generator) ->
          (try [Litint (StringMap.find generator env.function_index)]
           with Not_found -> raise (Failure ("undeclared function " ^ generator))) 
          @ expr height @ expr width @ [Litint 5]

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
                    (initializeEmptyArray 1301 []) @ [Litint 8]
              |   "Player" ->
                    (initializeEmptyArray 1101 []) @ [Litint 9]
              |   "Map" -> 
                    (initializeEmptyArray 701 []) @ [Litint 10]
              | _ -> raise (Failure ("Invalid array type " ^ array_type)) 
            )
      | AAccess(a, i) -> 
          expr i @ 
          (try [Lfpa (StringMap.find a env.local_index)]
          with Not_found -> try[Loda (StringMap.find a env.global_index)]
          with Not_found -> raise (Failure ("undeclared variable" ^ a)))
      | AAssign(a, i, e) ->
          expr e @ expr i @
          (try [Litint (StringMap.find a env.local_index)] @ [Sfpa]
          with Not_found -> try [Litint (StringMap.find a env.global_index)] @ [Stra]
          with Not_found -> raise (Failure ("undeclared variable" ^ a)))
      | Binop (e1, op, e2) -> expr e1 @ expr e2 @ [Bin op]
      | Not (e) -> 
        (*TODO:below current errors*)
        if expr e then [Litint 0] else [Litint 1]
      | Assign (s, e) ->
          expr e @
          try [Sfp (StringMap.find s env.local_index)]
          with Not_found -> try [Str (StringMap.find s env.global_index)]
          with Not_found -> raise (Failure ("undeclared variable " ^ s))
      | Call (fname, actuals) -> 
          if (fname = "Run") then
            let actualVars = expr actuals in
            if (List.length actualVars) <> 2 then raise(Failure("The function run expects 2 parameters.")) else
            let loadMap = [List.hd actualVars]
            and loadPlayer = [List.nth 1 (expr actuals)] in
            let whilebody = (loadPlayer @ [CheckUserInput]
                            @ (
                                let strPlayer = (match actuals with
                                                    hd :: tl -> (match (List.nth tl 0) with
                                                                    Id(x) -> expr Id(x)
                                                                    AAccess(a, i) -> expr AAccess(a, i)
                                                                )) in
                                strPlayer
                              )
                            @ loadPlayer @ (expr Call("DrawPlayer", [List.nth actuals 1])) @ (expr Call("CallGenerator", [List.nth actuals 0]))) in
            [loadMap] @ [OpenWin] @ [Bra (List.length whilebody)] @ whilebody @ [CheckCollision] @ [Bne -((List.length whilebody) + 1)]
          else
          (try
           (List.concat (List.map expr (List.rev actuals))) @
           (try [Jsr (StringMap.find fname env.function_index)]   
            with Not_found -> raise (Failure ("undefined function " ^ fname)))
      | Noexpr -> []

    in let rec stmt = function
        Block sl     ->  List.concat (List.map stmt sl)
      | Expr e       -> expr e @ [Drp]
      | Return e     -> expr e @ [Rts num_formals]
      | If (p, t, f) -> let t' = stmt t and f' = stmt f in
                         (expr p @ [Beq(2 + List.length t')] @
                         t' @ [Bra(1 + List.length f')] @ f')
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
        try [(*OpenWin;*) Jsr (StringMap.find "$main" function_indexes); Hlt]
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
