infix $%;

fun  (x $% []) = false 
  |  (x $% y::ys) = x=y orelse x $% ys;
1 $% [~1,0,2,3,4,5,6];

fun intersect ([],[]) = [] 
  | intersect (xs,[]) = xs 
  | intersect([],ys) = [] 
  | intersect (x::xs,ys) = 
  if x $% ys 
    then x::intersect(xs,ys)
    else intersect (xs,ys);

intersect ([3,1,2,4],[1,2,3,4,5,6,7,8,9,10]);

fun subtract([],[]) = [] 
  | subtract(xs,[]) = [] 
  | subtract([],ys) = [] 
  | subtract(x::xs,ys) = 
  if not(x $% ys) 
    then x::subtract(xs,ys) 
    else subtract(xs,ys);


subtract ([1,2,3,4,5],[3,2,1,4]);
subtract ([1,2,3],[4,5,6]);
subtract (["Anne","Mary","Rosemary"],["Mary"]);


fun union ([],[]) = []
  | union (xs , []) = xs
  | union ([], ys) = ys
  | union (x::xs, ys) = 
  if not(x $% ys) 
  then x::union (xs,ys) 
  else x::union (xs, subtract 
                (ys, intersect (x::xs, ys))
                );


