function $main : void ()
{
  $printstring("Test if else. Should print 8 9 17");
  if (1) $printint(8); else $printint(42);
  if (0) $printint(41); else $printint(9);
  $printint(17);
}