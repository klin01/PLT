/* Bug noticed by Pin-Chin Huang */

function $fun : int (int $x, int $y)
{
  return 0;
}

function $main : void ()
{
  int $i;
  $i : 1;

  $printstring("I am not sure");
  $fun($i:5, $i:$i + 2);

  $printint( $i );

}

