open Thread

(* Somehow without line 4 I get an error... *)
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
};;

(* Convert (r,g,b) into a single OCaml color value c *)
let color_from_rgb r g b =
  r*256*256 + g*256 + b;
in

(*
  Relatively translate all vertex given the translation distance ex
*)
let rec trans_allVertices_x ex = function
  [] -> []
  | px::py::tl -> (px + ex)::(py::(trans_allVertices_x ex tl));
in

(*
  Relatively translate all vertex given the translation distance ey
*)
let rec trans_allVertices_y ey = function
  [] -> []
  | px::py::tl ->(px)::((py + ey)::(trans_allVertices_y ey tl));
in

(*
  Given absolute location in x of the first vertex of the polygon,
  rigidly translate all vertex relative to this absolute location
*)
let trans_allVertices_abs_x abx vlist =
  let distant = abx - (List.nth vlist 0) in
    let rec trans_abs_x dist = function
      [] -> []
      | px::py::tl -> (px + dist)::(py::(trans_abs_x dist tl)) in
        trans_abs_x distant vlist;
in

(*
  Given absolute location in y of the first vertex of the polygon,
  rigidly translate all vertex relative to this absolute location
*)
let trans_allVertices_abs_y aby vlist =
  let distant = aby - (List.nth vlist 1) in
    let rec trans_abs_y dist = function
      [] -> []
      | px::py::tl -> (px)::((py + dist)::(trans_abs_y dist tl)) in
        trans_abs_y distant vlist;
in

(* Given the list of vertices *)
let rec find_max_y current = function
    []           -> current
    | px::py::tl -> if (py > current) then (find_max_y py tl) else (find_max_y current tl); in

let rec find_min_y current = function
    []           -> current
    | px::py::tl -> if (py < current) then (find_min_y py tl) else (find_min_y current tl); in

let rec find_max_x current = function
    []           -> current
    | px::py::tl -> if (px > current) then (find_max_x px tl) else (find_max_x current tl); in

let rec find_min_x current = function
    []           -> current
    | px::py::tl -> if (px < current) then (find_min_x px tl) else (find_min_x current tl); in


(*(* Draw the player! *)
let draw_player vlist color =
  Graphics.set_color color;
  Graphics.fill_rect (x) (y) size size;
in

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
  in
  Graphics.fill_poly (Array.of_list (buildTupleArray vlist));
in)
*)

(*
  Given a list of vertex coordinates [x0, y0, x1, y1, ...] and color,
  draw and fill the polygon. 
  *)
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
  in
  Graphics.fill_poly (Array.of_list (buildTupleArray vlist));
in

(* Draw the moving block *)
let draw_rectangle x y size color =
  Graphics.set_color color;
  Graphics.fill_rect (x) (y) size size;
  in

let draw_string x y str =
  Graphics.moveto x y;
  Graphics.set_text_size 30;
  Graphics.draw_string str;
  in

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
  let objectheight = (max_y - (List.nth s.playerData.player_vertices 1)) in

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
      |'z'   -> if min_y > 0 then 
                  s.playerData.player_vertices <- 
                  (trans_allVertices_y (-15) s.playerData.player_vertices)
                else
                  s.playerData.player_vertices <- 
                  (trans_allVertices_abs_y 0 s.playerData.player_vertices)
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
(*
    let block_min_x = find_min_x s.winWidth block.block_vertices
    and block_max_x = find_max_x 0 block.block_vertices
    and player_min_x = find_min_x s.winWidth s.playerData.player_vertices
    and player_max_x = find_max_x 0 s.playerData.player_vertices

    and block_min_y = find_min_y s.winHeight block.block_vertices
    and block_max_y = find_max_y 0 block.block_vertices
    and player_min_y = find_min_y s.winHeight s.playerData.player_vertices
    and player_max_y = find_max_y 0 s.playerData.player_vertices in

      if (player_max_x > block_min_x && player_min_x < block_min_x) then
        let _result = Clip.gpcml_clippolygon 
                      Clip.Intersection 
                      (makeGPCPolygon s.playerData.player_vertices) 
                      (makeGPCPolygon block.block_vertices) in
        (Clip.gpcml_isOverlapped _result)
      else 
        (if (((player_max_y > block_min_y) && (player_min_y < block_min_y)) || 
            ((block_max_y > player_min_y) && (block_min_y < player_min_y))) then
        let _result = Clip.gpcml_clippolygon 
                      Clip.Intersection 
                      (makeGPCPolygon s.playerData.player_vertices) 
                      (makeGPCPolygon block.block_vertices) in
        (Clip.gpcml_isOverlapped _result))
      in
*)
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

let skel f_init f_end f_key (*f_mouse*) f_updateFrame f_except f_playerCollided = 
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

let gameState = {winWidth=800; winHeight=600; 
                winBgColor=(color_from_rgb 255 255 255);
                blockData=blocks;
                reset=true;
                gravityFlag=0;
                playerData=player};
in

let slate () =
    skel (t_init gameState) (t_end gameState)
         (t_key gameState) (t_updateFrame gameState) 
         (t_except gameState) (t_playerCollided gameState); 
in

slate ();
print_endline("Game End!");;

(*

(*
  Draw rectangle start at (10,100), for size (x, 2x)
  The (0,0) is bottom left.
  For some reason we need to specify PARENTHESIS before the numbers themselves..
*)
let create_rectangle size x y r g b =
  Graphics.draw_rect(x) (y) (size) (2*size);
  (*Graphics.draw_circle(x+100) (y+100) (size/2);*)
  Graphics.set_color( color_from_rgb r g b );
  Graphics.fill_rect(x) (y) (size) (2*size)
  in


(*
  ANIMATION YAY! First version: using for loop
  Advantage: Easy to use. Don't have to worry about mutable int
  Disadvantage: Cannot increment any other value than 1.
*)
let move_steps1 total step size = 
  for i = 0 to total do 
      Graphics.clear_graph ();
      create_rectangle size (10) (200 - i) 255 0 0 ;
      create_rectangle (size*2/3) (100) (400 - i) 123 12 200;
      Thread.join(Thread.create(Thread.delay)(1. /. 24.)) done;
  in

(*
  ANIMATION YAY! Second version: using while loop
  Advantage: Can increment by arbitrary value. Here I implemented "step".
  Disadvantage: Have to mess with mutable data. Shown below.
*)
let move_steps2 total step size = 
  let i = ref 0 in
  while !i < total do 
      Graphics.clear_graph ();
      create_rectangle size (10) (200 - !i) 255 0 0 ;
      create_rectangle (size*2/3) (100) (400 - !i) 123 12 200;
      i := !i + step;
      Thread.join(Thread.create(Thread.delay)(1. /. 24.)) (* NOTICE NO semicolon before done;*)
      done;
  in

(*
  For some reason when we initialize size of the screen we will need
  a space before WIDTH is defined, otherwise we will get an error!
*)
let x = 200 in
  Graphics.open_graph " 600x800";

  move_steps2 100 3 x;

  Thread.join(Thread.create(Thread.delay)(float_of_int 4000 /. 1000.0));
  Graphics.clear_graph ();;

*)