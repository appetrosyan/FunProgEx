(* created by A. Petrosyan as part of supervision work*)
(* on 11/05/2016*)

fun Quadrant (x:real,y:real) = 
    if x>0.0
      then 
          if y>0.0 
            then 0.0 (*First Quadrant*)
            else 3.0 (*Fourth Quadrant*)
      else
          if y>0.0
            then 1.0 (*Second Quadrant*)
            else 2.0 (*Third Quadrant*)
;
infix within;
fun ((x:real) within ((a:real), (b:real)))=
    x<=b andalso x>=a;
exception mathError;

(*An arctan in the first quadrant*)
fun arctan x:real = 
    if x within (0.9999999,1.00000001) (*Special case *)    
    then Math.pi/4.0 
    else
        if x within (0.0, Math.pi/4.0)
        then 
            ( Math.pi/4.0 + 0.273*(1.0 - x) )*x
        else
            if x within (0.0, Math.pi/2.0)
            then
              (arctan (1.0/x) - Math.pi/2.0)
            else 
              raise mathError;

          (*Produces an angle within [0, 2\pi]*)    
fun atan2 (y:real, x:real) = 
    arctan (abs(y/x)) + Quadrant(x,y)*Math.pi/2.0; 

val tests = [(~1.0,1.0),(52314.0,78785.135),(~0.00001,0.00001)];
val library = map Math.atan2 tests;
val my = map atan2 tests;
val discrepancies = map Real.- (ListPair.zip (my, library));

          
            
