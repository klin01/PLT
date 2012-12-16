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
let create_rectangle x r g b =
  Graphics.draw_rect(10) (100) (x) (2*x);
  Graphics.draw_circle(200) (200) (x/2);
  Graphics.set_color( color_from_rgb r g b );
  Graphics.fill_rect(10) (100) (x) (2*x)
  in

(*
  For some reason when we initialize size of the screen we will need
  a space before WIDTH is defined, otherwise we will get an error!
*)
let x = 200 in
  Graphics.open_graph " 600x800";
  create_rectangle x 255 0 0 ;
  create_rectangle (x*2/3) 123 12 200 ;

  Thread.join(Thread.create(Thread.delay)(float_of_int 4000 /. 1000.0));
  Graphics.clear_graph ();;