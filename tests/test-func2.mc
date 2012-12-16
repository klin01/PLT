/* Bug noticed by Pin-Chin Huang */

function $fun : int (int $x, int $y)
{
  return 0;
}

function $main : void ()
{
  int $i;
  $i = 1;

  $printstring("I am not sure what it should print.. 1? 2? 3?");
  $fun($i = 2, $i = $i + 1);

  $printint( $i );

}

