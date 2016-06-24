fun map2 f acc [] = [] 
|   map2 f acc ([]::xs) = (rev acc) :: map2 f [] xs 
|   map2 f acc ((x::xs)::xss) = map2 f ((f x )::acc) (xs::xss);

fun map f []  = [] 
  | map f (x::xs) = (f x) :: map f xs;

  
