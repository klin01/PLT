function $gcd : int (int $a, int $b) {
	
  return $a;
}

function $main : void ()
{
	$printstring("Should print 2, 3, and 11");
  	$printint( $gcd(2,14) );
  	$printint( $gcd(3,15) );
  	$printint( $gcd(99,121) );
}