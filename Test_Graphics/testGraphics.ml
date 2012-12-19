open Thread

(* Somehow without line 4 I get an error... *)
exception End;;

type blockType = {
  mutable block_x:int; 
  mutable block_y:int; 
  mutable block_size:int; 
  mutable block_color:int;
};;

type state = {
  mutable winWidth:int; 
  mutable winHeight:int; 
  mutable winBgColor:int;

  mutable blockData:blockType list;

  mutable player_x:int; 
  mutable player_y:int;
  mutable player_size:int; 
  mutable player_color:int;
};;

(* Convert (r,g,b) into a single OCaml color value c *)
let color_from_rgb r g b =
  r*256*256 + g*256 + b;
in

(* Draw the player! *)
let draw_player x y size color =
  Graphics.set_color color;
  Graphics.fill_rect (x) (y) size size;
in

(* Draw the moving block *)
let draw_rectangle x y size color =
  Graphics.set_color color;
  Graphics.fill_rect (x) (y) size size;
  in

let draw_string x y str =
  Graphics.moveto x y;
  Graphics.set_text_size 300;
  Graphics.draw_string str;
  in

(* s is state *)
let t_init s () =
  Graphics.open_graph (" " ^ (string_of_int s.winWidth) ^ "x" ^
                             (string_of_int s.winHeight));
  Graphics.set_color s.winBgColor;
  Graphics.fill_rect 0 0 s.winWidth s.winHeight;
  (*Graphics.set_color s.player_color;*)
  draw_player s.player_x s.player_y s.player_size s.player_color;
  (*Graphics.set_color s.block1_color;*)
  
  List.iter (fun block -> (draw_rectangle block.block_x
                                          block.block_y
                                          block.block_size
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
  (match c with
    ' '   -> if s.player_y < s.winHeight - s.player_size then s.player_y <- s.player_y + 2;
  | 'z'   -> if s.player_y > 0 then s.player_y <- s.player_y - 3;
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

    List.iter (fun block -> (block.block_x <- block.block_x - 3)) s.blockData;

    List.iter (fun block -> (draw_rectangle block.block_x
                                          block.block_y
                                          block.block_size
                                          block.block_color)) s.blockData;

  draw_player s.player_x s.player_y s.player_size s.player_color;
in
  

let t_except s ex = ();
in

let t_playerCollided s () = 
  let check block =
    if ((s.player_x + s.player_size > block.block_x) && (s.player_x < block.block_x)) then
      if (((s.player_y + s.player_size > block.block_y) && (s.player_y < block.block_y)) || 
            ((block.block_y + block.block_size > s.player_y) && (block.block_y < s.player_y))) then
        true
      else false
    else false in
  (*let collisionList = List.Map (fun block ->
      if (s.player_x + s.player_size < block.block_x) then
        if (((s.player_y + s.player_size > block.block_y) && (s.player_y < block.block_y)) || 
            ((block.block_y + block.block_size > s.player_y) && (block.block_y < s.player_y)))
        then true else false;
      else false) s.blockData;*)
  let result list = List.fold_left (fun a b -> a || b) false list in
    let collisionList = List.map check s.blockData in
      (*print_endline (string_of_bool (result collisionList));*)
      result collisionList;
in


(*let i = ref 0; in*)

let skel f_init f_end f_key (*f_mouse*) f_updateFrame f_except f_playerCollided = 
  f_init ();
  try 
      while true do
        Thread.join(Thread.create(Thread.delay)(1.0 /. 24.0));
        f_updateFrame ();
        try 

          if Graphics.key_pressed () then f_key (Graphics.read_key ());
          if f_playerCollided () then f_end ();

        with 
             End -> raise End
           |  e  -> f_except e
      done
  with 
      End  -> f_end ();

in

let block1 = { block_x=500; block_y=200; block_size=150; 
                block_color=(color_from_rgb 20 20 20) }; in
let block2 = { block_x=600; block_y=500; block_size=100; 
                block_color=(color_from_rgb 150 20 120) }; in
let block3 = { block_x=400; block_y=0; block_size=50; 
                block_color=(color_from_rgb 20 120 20) }; in

let blocks = [block1; block2; block3];
in

let gameState = {winWidth=800; winHeight=600; 
                winBgColor=(color_from_rgb 255 255 255);
                blockData=blocks;
                player_x=50; player_y=300; player_size=50;
                player_color=(color_from_rgb 123 12 200);};
in

let slate () =
    skel (t_init gameState) (t_end gameState)
         (t_key gameState) (t_updateFrame gameState) 
         (t_except gameState) (t_playerCollided gameState); 
in

slate ();
print_endline("Hello!");;

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