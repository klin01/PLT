function $main : void ()
{
  int $i;
  $printstring("Should print 1 to 4");
  for ($i = 0 ; $i < 5 ; $i = $i + 1) {
    $printint( $i );
  }

  $printstring("Should print 5 to 9");
  for ($i = 5 ; $i < 10 ; $i += 1) {
    $printint( $i );
  }
}
