function $main : void ()
{
	Array int $myInt;
	int $i;

	$printstring("Local array, prints 7 21 9");

	$myInt : new Array int;
	
	$myInt[0] : 7;
	$myInt[1] : 21;
	$myInt[99] : 9;

	$printint( $myInt[0] );
	$printint( $myInt[1] );
	$printint( $myInt[99] );
}
