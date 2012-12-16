function $add : int (int $a, int $b)
{
  return $a + $b;
}

function $main : void ()
{
  int $a;
  $a = $add(39, 3);
  $printstring("Should print 42");
  $printint( $a );
}
