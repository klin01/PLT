Array int $sum;

function $plusArray : void (Array int $a1, Array int $a2) {
	int $i;
	$sum : new Array int;
	for ($i : 0; $i < 2; $i+:1) {
		$sum[$i] : $a1[$i] + $a2[$i];
	}
}

function $main : void ()
{
	Array int $c;
	Array int $d;
	int $j;

	$printstring("Func w array input, prints 55 110");
	$printstring("Use global array for func return");

	$c : new Array int;
	$d : new Array int;

	$c[0] : 33;
	$d[0] : 22;
	$c[1] : 66;
	$d[1] : 44;

	$plusArray($c,$d);
	
	$printint($sum[0]);		
	$printint($sum[1]);		
}