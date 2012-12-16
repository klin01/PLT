function main : void ()
{

	$printstring("Should print 5 4 3 2 1 42")

  int $i;
  $i = 5;
  while ($i > 0) {
    $printint( $i );
    $i = $i - 1;
  }
  $printint(42);
}
