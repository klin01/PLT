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
  $a = $a + 1;
  $b = $b + 1;
}

function $main : void ()
{
  $printstring("Test global var. Should print 43 22, then 44 23");
  $a = 42;
  $b = 21;
  $printa();
  $printb();
  $incab();
  $printa();
  $printb();
}
