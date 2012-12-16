int $globalvar1;
int $globalvar2;

//$globalvar2 : 1;
//$globalvar1 : 2;

function $generateThis: Array Brick () {
  //vdecls
  Array Brick $output;
  Brick $top;
  Brick $bottom;
  //stmts
  $output : new Array Brick;

  $top : new Brick(\"red\", 20, 1024, 0, 1004);
  $output->$push($top);
  $bottom : new Brick(\"red\", 20, 1024, 0, 0);
  $output->$push($bottom);
  return $output;
}

function $main : void () {
  
  Map $gameMap;
  Player $me;
  int $test;

  $test: 5;
  $gameMap: new Map(1024, 768, $generateThis);
  $me : new Player(\"red\", \"triangle\", 20, 20, 500);

  $Run($gameMap, $me);
}