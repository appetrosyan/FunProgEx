exception nsq;

fun sq x:real = x*x;

fun s_q (a:real,b:real,c:real) =
  let val D = (sq b) - 4.0*a*c
      val x1 = (~b+Math.sqrt(D))/(2.0*a)
      val x2 = (~b-Math.sqrt(D))/(2.0*a)
  in
      if D>=0.0
      then (x1,x2)
      else raise nsq
  end;

