function $printem : void (int $a, int $b, int $c, int $d)
{
  $printint($a);
  $printint($b);
  $printint($c);
  $printint($d);
}

function $main : void ()
{
  $printem(42,17,192,8);
}