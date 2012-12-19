open Thread

(* Somehow without line 4 I get an error... *)
exception End;;


let color_from_rgb r g b =
  r*256*256 + g*256 + b 
  in

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