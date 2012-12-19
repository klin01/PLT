open Ast
open Bytecode

let array_def_size = 100
let a = 1

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

(*
  Storage convention for references:
  Local:
  -1
  <address>
  Global:
  1
  <address>
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
        (n + 5, hd.varname ^ ".$width") ::
        (n + 6, hd.varname) :: enum stride (n+stride * 7) tl 
          (* Map size : 1 * 2 int for generator function, 2 x 2 int (h, w), 1 for type (5) = 7 *)
      | "Arrayint" ->   (n + 2*array_def_size, hd.varname) :: enum stride (n+stride * 2 * array_def_size + 1) tl
      | "Arraystring" -> (n + 40*array_def_size, hd.varname) :: enum stride (n+stride * 40 * array_def_size + 1) tl
      | "ArrayBrick" ->  (n + 13*array_def_size, hd.varname) :: enum stride (n+stride * 13 * array_def_size + 1) tl
      | "ArrayPlayer" -> (n + 11*array_def_size, hd.varname) :: enum stride (n+stride * 11 * array_def_size + 1) tl
      | "ArrayMap" ->    (n + 7*array_def_size, hd.varname) :: enum stride (n+stride * 7 * array_def_size + 1) tl
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
        (n - 3, hd.varname ^ ".$height" ) ::
        (n - 1, hd.varname ^ ".$width") ::
        (n, hd.varname) :: enum stride (n+stride * 7) tl 
      | "Arrayint" ->   (n, hd.varname) :: enum stride (n+stride * 2 * array_def_size - 1) tl
      | "Arraystring" -> (n, hd.varname) :: enum stride (n+stride * 40 * array_def_size - 1) tl
      | "ArrayBrick" ->  (n, hd.varname) :: enum stride (n+stride * 13 * array_def_size - 1) tl
      | "ArrayPlayer" -> (n, hd.varname) :: enum stride (n+stride * 11 * array_def_size - 1) tl
      | "ArrayMap" ->    (n, hd.varname) :: enum stride (n+stride * 7 * array_def_size - 1) tl
      | _ -> raise(Failure ("Undefined type with variable" ^ hd.varname))

let rec enumInitCommands stride n isLocal = function
    [] -> []
  | hd::tl -> 
    if stride > 0 then
      match hd.vartype with
        "int" ->    
        (Init (1, (n + 1), isLocal)) :: 
        enumInitCommands stride (n+stride * 2) isLocal tl
      | "string" -> 
        (Init (2, (n + 39), isLocal)) :: 
        enumInitCommands stride (n+stride * 40) isLocal tl 
      | "Brick" ->
        (Init (1, (n + 1), isLocal)) :: (* hd.varname ^ ".$y" *)
        (Init (1, (n + 3), isLocal)) :: (* hd.varname ^ ".$x" *)
        (Init (-1, (n + 5), isLocal)) :: (* hd.varname ^ ".$vertices" *)
        (Init (1, (n + 7), isLocal)) :: (* hd.varname ^ ".$colorB" *)
        (Init (1, (n + 9), isLocal)) :: (* hd.varname ^ ".$colorG" *)
        (Init (1, (n + 11), isLocal)) :: (* hd.varname ^ ".$colorR" *)
        (Init (3, (n + 12), isLocal)) ::
        enumInitCommands stride (n+stride * 13) isLocal tl
          (* Brick size : 3 * 2 int (color), 1 * 2int for vertex array, 2 * 2 for x and y, 1 int for type (3) = 13 *) 
      | "Player" -> 
        (Init (1, (n + 1), isLocal)) :: (* hd.varname ^ ".$y" *)
        (Init (-1, (n + 3), isLocal)) :: (* hd.varname ^ ".$vertices" *)
        (Init (1, (n + 5), isLocal)) :: (* hd.varname ^ ".$colorB" *)
        (Init (1, (n + 7), isLocal)) :: (* hd.varname ^ ".$colorG" *)
        (Init (1, (n + 9), isLocal)) :: (* hd.varname ^ ".$colorR" *)
        (Init (4, (n + 10), isLocal)) ::
        enumInitCommands stride (n+stride * 11) isLocal tl 
          (* Player size :  3 * 2 int (color), 1 * 2 int for vertex array, 1 * 2 int (y), 1 for type (4) = 11 *)
      | "Map" ->    
        (Init (-1, (n + 1), isLocal)) :: (* hd.varname ^ ".$generator" *)
        (Init (1, (n + 3), isLocal)) :: (* hd.varname ^ ".$height" *)
        (Init (1, (n + 5), isLocal)) :: (* hd.varname ^ ".$width" *)
        (Init (5, (n + 6), isLocal)) :: 
        enumInitCommands stride (n+stride * 7) isLocal tl 
          (* Map size : 1 * 2 int for generator function, 2 x 2 int (h, w), 1 for type (5) = 7 *)
      | "Arrayint" ->   
        (Init (6, (n + 2*array_def_size), isLocal)) ::
        enumInitCommands stride (n+stride * 2 * array_def_size + 1) isLocal tl
      | "Arraystring" -> 
        (Init (7, (n + 40*array_def_size), isLocal)) :: 
        enumInitCommands stride (n+stride * 40 * array_def_size + 1) isLocal tl
      | "ArrayBrick" ->  
        (Init (8, (n + 13*array_def_size), isLocal)) :: 
        enumInitCommands stride (n+stride * 13 * array_def_size + 1) isLocal tl
      | "ArrayPlayer" -> 
        (Init (9, (n + 11*array_def_size), isLocal)) :: 
        enumInitCommands stride (n+stride * 11 * array_def_size + 1) isLocal tl
      | "ArrayMap" ->    
        (Init (10, (n + 7*array_def_size), isLocal)) :: 
        enumInitCommands stride (n+stride * 7 * array_def_size + 1) isLocal tl
      | _ -> raise(Failure ("Undefined type with variable" ^ hd.varname))
    else
      match hd.vartype with
        "int" ->    
        (Init (1, n, isLocal)) :: 
        enumInitCommands stride (n+stride * 2) isLocal tl
      | "string" -> 
        (Init (2, n, isLocal)) :: 
        enumInitCommands stride (n+stride * 40) isLocal tl 
      | "Brick" ->  
        (Init (1, (n - 11), isLocal)) :: (* hd.varname ^ ".$y" *)
        (Init (1, (n - 9), isLocal)) :: (* hd.varname ^ ".$x" *)
        (Init (-1, (n - 7), isLocal)) :: (* hd.varname ^ ".$vertices" *)
        (Init (1, (n - 5), isLocal)) :: (* hd.varname ^ ".$colorB" *)
        (Init (1, (n - 3), isLocal)) :: (* hd.varname ^ ".$colorG" *)
        (Init (1, (n - 1), isLocal)) :: (* hd.varname ^ ".$colorR" *)
        (Init (3, (n), isLocal)) :: 
        enumInitCommands stride (n+stride * 13) isLocal tl 
      | "Player" -> 
        (Init (1, (n - 9), isLocal)) :: (* hd.varname ^ ".$y" *)
        (Init (-1, (n - 7), isLocal)) :: (* hd.varname ^ ".$vertices" *)
        (Init (1, (n - 5), isLocal)) :: (* hd.varname ^ ".$colorB" *)
        (Init (1, (n - 3), isLocal)) :: (* hd.varname ^ ".$colorG" *)
        (Init (1, (n - 1), isLocal)) :: (* hd.varname ^ ".$colorR" *)
        (Init (5, (n), isLocal)) :: 
        enumInitCommands stride (n+stride * 11) isLocal tl 
      | "Map" ->    
        (Init (-1, (n - 5), isLocal)) :: (* hd.varname ^ ".$generator" *)
        (Init (1, (n - 3), isLocal)) :: (* hd.varname ^ ".$height" *)
        (Init (1, (n - 1), isLocal)) :: (* hd.varname ^ ".$width" *)
        (Init (5, (n), isLocal)) :: 
        enumInitCommands stride (n+stride * 7) isLocal tl 
      | "Arrayint" ->   
        (Init (6, n, isLocal)) :: 
        enumInitCommands stride (n+stride * 2 * array_def_size - 1) isLocal tl
      | "Arraystring" -> 
        (Init (7, n, isLocal)) :: 
        enumInitCommands stride (n+stride * 40 * array_def_size - 1) isLocal tl
      | "ArrayBrick" ->  
        (Init (8, n, isLocal)) :: 
        enumInitCommands stride (n+stride * 13 * array_def_size - 1) isLocal tl
      | "ArrayPlayer" -> 
        (Init (9, n, isLocal)) :: 
        enumInitCommands stride (n+stride * 11 * array_def_size - 1) isLocal tl
      | "ArrayMap" ->    
        (Init (10, n, isLocal)) :: 
        enumInitCommands stride (n+stride * 7 * array_def_size - 1) isLocal tl
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
  let globalinits = enumInitCommands 1 0 0 globals in
  (* Assign indexes to function names *)
  let built_in_functions = StringMap.add "$DrawPlayer" (-1) StringMap.empty in
  let built_in_functions = StringMap.add "$Run" (-2) built_in_functions in
  let built_in_functions = StringMap.add "$printint" (-3) built_in_functions in
  let built_in_functions = StringMap.add "$printstring" (-4) built_in_functions in
  let built_in_functions = StringMap.add "$dumpstack" (-5) built_in_functions in
  let built_in_functions = StringMap.add "$CallGenerator" (-6) built_in_functions in
  let built_in_functions = StringMap.add "$Push" (-7) built_in_functions in
  let built_in_functions = StringMap.add "$GetCurrentScore" (-8) built_in_functions in
  let built_in_functions = StringMap.add "$GenerateRandomInt" (-9) built_in_functions in
  let built_in_functions = StringMap.add "$ArrayCount" (-10) built_in_functions in
  
  let function_indexes = string_map_pairs built_in_functions
      (enum_func 1 1 (List.map (fun f -> f.fname) functions)) in

  (* Translate a function in AST form into a list of bytecode statements *)
  let translate env fdecl =
    (* Bookkeeping: FP offsets for locals and arguments *)
    let num_formals = total_varsize 0 fdecl.formals
    and num_locals = total_varsize 0 fdecl.locals
    and local_offsets = enum 1 1 fdecl.locals
    and formal_offsets = enum (-1) (-2) fdecl.formals in
    let localinits = enumInitCommands 1 1 1 fdecl.locals
    and formalinits = enumInitCommands (-1) (-2) 1 fdecl.formals in
    let env = { env with local_index = string_map_pairs
      StringMap.empty (local_offsets @ formal_offsets) } in

    let rec expr = function
        LiteralInt i -> [Litint i] 
      | LiteralString i -> [Litstr i]
      | Id s ->
          let sub = (if (String.length s) > 10 then String.sub s ((String.length s)-10) 10 else s) in
              if sub = ".$vertices" then (* Account for case where only a reference to an array is stored i.e. $brick.$vertices *)
                (try [Litint (StringMap.find s env.local_index)] @ [Lfp (-32769)]
                 with Not_found -> try [Litint (StringMap.find s env.global_index)] @ [Lod (-32769)]
                 (*with Not_found -> try [Lodf (StringMap.find s env.function_index)]*)
                 with Not_found -> raise (Failure ("undeclared Id " ^ s)))
              else
                (try [Lfp (StringMap.find s env.local_index)]
                 with Not_found -> try [Lod (StringMap.find s env.global_index)]
                 with Not_found -> try [Litint (StringMap.find s env.function_index)]
                 with Not_found -> raise (Failure ("undeclared Id " ^ s)))

      | Brick (r, g, b, varray, x, y) ->
          expr y @ expr x 
          @ (try [Litint (StringMap.find varray env.local_index)] @ [Make 0] @ [Litint (-1)] @ [Make 0]
             with Not_found -> try [Litint (StringMap.find varray env.global_index)]
             with Not_found -> raise (Failure ("undeclared Brick " ^ varray)))
          @ expr b @ expr g @ expr r
          @ [Litint 3] @ [Make 3]

      | Player (r, g, b, varray, y) ->
          expr y @ (try [Litint (StringMap.find varray env.local_index)] @ [Make 0] @ [Litint (-1)] @ [Make 0]
                    with Not_found -> try [Litint (StringMap.find varray env.global_index)]
                    with Not_found -> raise (Failure ("undeclared Player " ^ varray)))
          @ expr b @ expr g @ expr r
          @ [Litint 4] @ [Make 4]

      | Map (width, height, generator) ->
          (*(StringMap.iter (fun a b -> print_endline (string_of_int b)) env.function_index);*)
          (try [Litf (StringMap.find generator function_indexes)]
           with Not_found -> raise (Failure ("undeclared function " ^ generator))) 
          @ expr height @ expr width @ [Litint 5] @ [Make 5]

      | Array (array_type) -> (* Push an empty array onto stack with type identifier on top *)
          (match array_type with
                "int" -> [Make 6]
            |   "string" -> [Make 7]
            |   "Brick" -> [Make 8]
            |   "Player" -> [Make 9]
            |   "Map" -> [Make 10]
            | _ -> raise (Failure ("Invalid array type " ^ array_type)) 
          )
      | AAccess(a, i) -> 
          expr i @ 
          (try [Litint (StringMap.find a env.local_index)] @ [Lfpa]
          with Not_found -> try[Litint (StringMap.find a env.global_index)] @ [Loda]
          with Not_found -> raise (Failure ("AAccess: undeclared array " ^ a)))
      | AAssign(a, i, e) ->
          expr e @ expr i @
          (try [Litint (StringMap.find a env.local_index)] @ [Sfpa]
          with Not_found -> try [Litint (StringMap.find a env.global_index)] @ [Stra]
          with Not_found -> raise (Failure ("AAssign: undeclared array " ^ a)))
       | AAccessByRef(a, i) -> 
          expr i @ 
          (try [Lfp (StringMap.find a env.local_index)] @ [Lref]
          with Not_found -> try[Lod (StringMap.find a env.global_index)] @ [Lref]
          with Not_found -> raise (Failure ("AAccessByRef: undeclared array " ^ a)))
      | AAssignByRef(a, i, e) ->
          expr e @ expr i @
          (try [Lfp (StringMap.find a env.local_index)] @ [Sref]
          with Not_found -> try [Lod (StringMap.find a env.global_index)] @ [Sref]
          with Not_found -> raise (Failure ("AAssignByRef: undeclared array " ^ a)))
      | Binop (e1, op, e2) -> expr e1 @ expr e2 @ [Bin op]
      | Not(e) -> 
        expr e @ [Nt]
      | AssignToRef (s, e) ->
        let strLen = String.length s in 
        (match e with 
          Id(str) -> 
            let sub = String.sub s (strLen-10) 10 in
              if sub = ".$vertices" then
              (try [Litint (StringMap.find str env.local_index)] @ [Make 0] @ [Litint (-1)] @ [Make 0]
              with Not_found -> try [Litint (StringMap.find str env.global_index)]
              with Not_found -> raise (Failure ("undeclared Id " ^ str)))  
            else
            let sub2 = String.sub s (strLen-11) 11 in
              if sub2 = ".$generator" then
              (try [Litint (StringMap.find str env.local_index)] @ [Make 0] @ [Litint (-1)] @ [Make 0]
              with Not_found -> try [Litint (StringMap.find str env.global_index)]
              with Not_found -> raise (Failure ("undeclared Id " ^ str))) else expr e    
          | _ -> expr e
          
        ) @ (try [Sfp (StringMap.find s env.local_index)]
          with Not_found -> try [Str (StringMap.find s env.global_index)]
          with Not_found -> raise (Failure ("Assign: undeclared variable " ^ s)))

      | Assign (s, e) ->
           expr e @
          (try [Sfp (StringMap.find s env.local_index)]
          with Not_found -> try [Str (StringMap.find s env.global_index)]
          with Not_found -> raise (Failure ("Assign: undeclared variable " ^ s)))
    (*     | Ref(base, child) -> [Litstr child]
                               @ (try [Litint 1] @ [Litint (StringMap.find base env.local_index)]
                                  with Not_found -> try [Litint 2] @ [Litint (StringMap.find base env.global_index)]
                                  with Not_found -> raise (Failure ("undeclared variable " ^ base)))
                               @ [StrRef]) *)
      | Call (fname, actuals) -> 
          if (fname = "$Run") then
            let actualVars = (List.concat (List.map expr actuals)) in
            if (List.length actualVars) <> 2 then raise(Failure("The function run expects 2 parameters.")) else
            let loadMap = [List.hd actualVars]
            and loadPlayer = [List.nth actualVars 1] in
            let whilebody = (loadPlayer @ [CheckUserInput] @
                              (
                                let strPlayer = (match actuals with
                                                    hd :: tl -> (match (List.nth tl 0) with
                                                                    Id(x) -> expr (Assign(x,Noexpr))
                                                                  | AAccess(a, i) -> expr (AAssign(a, i, Noexpr))
                                                                  | _ -> raise(Failure("The second argument of run must be a reference to a player object.")))
                                                 | [] -> raise(Failure("Run must be applied to two arguments."))
                                                ) in
                                strPlayer
                              )
                            @ [Drp] @ (expr (Call("$DrawPlayer", [List.nth actuals 1]))) 
                            @ (expr (Call("$CallGenerator", [List.nth actuals 0])))) @ [ProcessBlocks] @ [Drp] @ [PrintScore] in
            loadMap @ [OpenWin] @ [Bra ((List.length whilebody)+1)] @ whilebody @ loadPlayer @ [CheckCollision] @ [Bne (-((List.length whilebody) + 2))]
          else
          (if (fname = "$Push") then
            let actualBytes = (List.concat (List.map expr (List.rev actuals))) in
            [List.nth (List.tl actualBytes) 0] @ (match (List.nth (List.tl actualBytes) 0) with
                Lod x -> [Litint 0] @ [Litint x]
             |  Lfp x -> [Litint 1] @ [Litint x]
             |  _ -> raise(Failure("Invalid array specified for Push function."))) 
            @ [(List.hd actualBytes)] @ [Jsr (-7)] @ (let array_name = (match actuals with
                                                                         hd :: tl -> (match hd with
                                                                                          Id(x) -> x                                                                                                            
                                                                                        | _ -> raise(Failure("The first argument of $Push must be a reference to an array.")))
                                                                       | [] -> raise(Failure("Run must be applied to two arguments."))
                                                                      ) in
                                                      (try [Litint (StringMap.find array_name env.local_index)] @ [Sfpa]
                                                       with Not_found -> try[Litint (StringMap.find array_name env.global_index)] @ [Stra]
                                                       with Not_found -> raise (Failure ("Attempt to push onto undeclared array " ^ array_name ^ ".")))
                                                    )
           else
           if (fname = "$GenerateRandomInt") then
              if (List.length actuals) <> 1 then raise(Failure("You must specify a single integer argument for the function $GenerateRandomInt.")) else
              expr (List.hd actuals) @ [Jsr (-9)]
           else
           ((StringMap.iter (fun a b -> print_endline (string_of_int b)) env.function_index);
           (List.concat (List.map expr (List.rev actuals))) @
           (try [Jsr (print_endline (fname ^ "--" ^ (string_of_int(StringMap.find fname env.function_index))); (StringMap.find fname env.function_index))]   
            with Not_found -> raise (Failure ("undefined function " ^ fname)))
          ))
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
       formalinits @ localinits @ 
       stmt (Block fdecl.body) @    (* Body *)
       [Litint 0; Rts num_formals]  (* Default = return 0 *)

    in let env = { function_index = function_indexes;
                   global_index = global_indexes;
                   local_index = StringMap.empty } in

  (* Code executed to start the program: Jsr main; halt *)
    let entry_function = 
        try [OpenWin] @ globalinits @ [Jsr (StringMap.find "$main" function_indexes); Hlt]
        with Not_found -> raise (Failure ("no \"$main\" function"))
    in
    
  (* Compile the functions *)
  let func_bodies = entry_function :: List.map (translate env) functions in

  (* Calculate function entry points by adding their lengths *)
  let (fun_offset_list, _) = List.fold_left
      (fun (l,i) f -> (i :: l, (i + List.length f))) ([],0) func_bodies in
  let func_offset = Array.of_list (List.rev fun_offset_list) in

  { globals_size = total_varsize 0 globals;
    (* Concatenate the compiled functions and replace the function
       indexes in Jsr statements with PC values *)
    text = Array.of_list (List.map (function 
                                      Jsr i when i > 0 -> Jsr func_offset.(i)
                                    | Litf i when i > 0 -> Litint func_offset.(i)
                                    | _ as s -> s) (List.concat func_bodies))
  }
