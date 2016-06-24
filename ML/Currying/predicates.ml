fun exists f [] = false
  | exists f (x::xs) = 
      f x orelse (exists f xs);

fun filter p [] = []
  | filter p (x::xs) = 
      if p x
      then 
          x:: filter p xs
      else 
          filter p xs;

