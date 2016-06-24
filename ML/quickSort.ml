fun quicksort [] = []
 |  quicksort [x] = [x]
 |  quicksort (x::xs) = 
        let fun partition (y, l, r, []) = (quicksort  l)@(y::(quicksort(r))) 
        | partition (y, l, r, z::zs) = 
                if z < y then partition ( y, z::l, r, zs) 
                else partition (y, l, z::r, zs) 
        in partition (x, [],[],xs)
        end; 

fun shortsort [] = [] 
 |  shortsort (x::xs) = 
         let val (left, right) = List.partition( fn y=> y<x) xs 
         in shortsort left @ (x:: shortsort right)
         end; 
