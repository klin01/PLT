
function $func : void (int $a)
{
	$printstring("func");
	$printint($a);
	if ($a)
		$printstring("True");
	else
		$printstring("False");
}


function $main : void ()
{
	$printstring("Should be 42");
	$printint(39 + 3);
	$func(0);
}
