function $fib : int (int $x)
{
  if ($x < 2) return 1;
  return $fib($x - 1) + $fib($x - 2);
}

function $main : void ()
{
	$printstring("Should be 1");
	$printint( $fib(0) );

	$printstring("Should be 1");
	$printint( $fib(1) );

	$printstring("Should be 2");
	$printint( $fib(2) );

	$printstring("Should be 3");
	$printint( $fib(3) );

	$printstring("Should be 5");
	$printint( $fib(4) );
	
	$printstring("Should be 8");
	$printint( $fib(5) );
}
