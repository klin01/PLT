function $main : void ()
{
  //$printstring("A lot of arithmetic tests");
  
  //$printstring("Should be 3, -1, 2, 50, 1, 99, -100");
  $printint(1 + 2);
  $printint(1 - 2);
  $printint(1 * 2);
  $printint(100 / 2);
  $printint(10 % 3);
  $printint(99);
  $printint(-100);

  //$printstring("Should be 0, 1, 99, 1, 0, 99");
  $printint(1 = 2);
  $printint(1 = 1);
  $printint(99);
  $printint(1 != 2);
  $printint(1 != 1);
  $printint(99);

  $printstring("Should be 1, 1, 0, 99, 0, 1, 99, 0, 11");
  $printint(1 < 2);
  $printint(2 < 1);
  $printint(99);
  $printint(1 <= 2);
  $printint(1 <= 1);
  $printint(2 <= 1);
  $printint(99);
  $printint(1 > 2);
  $printint(2 > 1);
  $printint(99);
  $printint(1 >= 2);
  $printint(1 >= 1);
  $printint(2 >= 1);

  $printstring("Bye Bye :3");
}
//Should be 1, 1, 0, 99, 0, 1, 99, 0, 111
