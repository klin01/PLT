open Ast

let string2expr str =
  let lexbuf = Lexing.from_string str in
  Parser.program Scanner.token lexbuf

let test =
 	string_of_program (string2expr("//timestep/distance/speed/score
function main : void () {
  //vdecls
  Map $gameMap;
  Player $me;
  //stmts
  $gameMap: Map {
    $height: 1024,
    $width: 768,
    $generator: $generateThis
  };
  $me : Player {
    $color: "red",
    $shape: "triangle",
    $height: 20,
    $width: 20,
    $y: 500
  }

  Run($gameMap, $me);
}

function $generateThis: Array () {
  //vdecls
  Array Brick $output;
  Brick $top;
  Brick $bottom;
  //stmts
  $output : Array Brick;

  $top : Brick {
    $height: 20,
    $width: 1024,
    $x: 0,
    $y: 1004
  };
  $output.push($top);
  $bottom : Brick {
    $height: 20,
    $width: 1024,
    $x: 0,
    $y: 0
  };
  $output.push($bottom);
  return $output;
}

//Array blocks = [];
//j = 0
//for (int i = 0; i < 500; i++)
//{
//  new Brick(i, 1, j, 0, red);
//  j++;
//}

/*Brick
  -height
  -width
  -x  
  -y
  -color */"))