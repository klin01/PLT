function $main : void ()
{
  int $i;
  $i : 5;
  $printstring("Should print 5 4 3 2 1 42");
  while ($i > 0) {
    $printint( $i );
    $i -: 1;
  }
  $printint(42);
}
