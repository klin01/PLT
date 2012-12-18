

function $main : void ()
{



	Array int $myInt;
	int $i;

	$myInt : new Array int;
	
	$myInt[0] = 12300;
	$printstring("myInt0:");
	$printint($myInt[0]);

	//$dumpstack();


/*	
	$myInt[1] = 2;
	$printstring("myInt1:");
	$printint($myInt[1]);	

	$myInt[2] = 4;
	$printstring("myInt2:");
	$printint($myInt[2]);	

	for($i : 0; $i < 3; $i +: 1) {
		$printint( $myInt[$i] );
	}
*/
}
