function $main : void ()
{
  $printstring("Test if else. Should print 42 8 17");
  if (1) $printint(42); else $printint(8);
  if (0) $printint(42); else $printint(8);
  $printint(17);
}
