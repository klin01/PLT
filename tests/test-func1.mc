function $add : int (int $a, int $b)
{
  return $a + $b;
}

function $main : void ()
{
  int $a;

  $printstring("func");
  $printint( $add(39,3) );

  $printstring("Should print 42");
}
