int $a;
int $b;

function $printa : void ()
{
  $printint( $a );
}

function $printb : void ()
{
  $printint( $b );
}

function $incab : void ()
{
  $a : $a + 1;
  $b : $b + 1;
}

function $main : void ()
{
  $printstring("Should print 43 22, then 44 23");
  $a : 43;
  $b : 22;
  $printa();
  $printb();
  $incab();
  $printa();
  $printb();
}
