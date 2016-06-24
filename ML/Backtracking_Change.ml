exception Change;
fun change (till, 0) = []
  | change ([], amt) = raise Change
  | change (c::till, amt) =
      if amt <0 then raise Change
      else (c::change ( c::till, amt-c))
       handle Change => change (till, amt); 
