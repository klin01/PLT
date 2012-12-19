(* Test for camlgpc. Create a rectangle and triangle and find their intersection (the region where they both exist). *)
let rectangle =
  Clip.gpc_polygon_of_box 0. 1. 0. 1.

let triangle =
  Clip.make_gpcpolygon
    [|false|]
      [|[|{Clip.x = 0.; Clip.y = 0.};
          {Clip.x = 0.5; Clip.y = 1.5};
          {Clip.x = 1.0; Clip.y = 0.}|]|]


let result =
  Clip.gpcml_clippolygon Clip.Intersection rectangle triangle

let _ =
  print_endline( string_of_int (Clip.gpcml_isOverlapped result));
	Clip.gpcml_printpolygon result