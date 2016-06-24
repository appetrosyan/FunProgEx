(*Functional arrays represent an array as a tree. The binary representation of the array's subscript is the branching structure, e.g. 1 means turn left, 0 means turn right. (leading zero means stop). 

Lookup and update are logarithmic, Accessing an element of the funcitonal array, does depend on  place (e.g. the root of the tree is accessed much faster, than a leaf)which means that this representation is not exact, i.e. it's different from real arrays.*)
datatype 'a tree = Lf
                 | Br of 'a * 'a tree * 'a tree;
infix $$;
fun v $$ Lf = Br(v, Lf, Lf)
  | v $$ (Br(w, t1,t2)) = Br (v, w $$ t2, t1);


fun arrayoflist [] = Lf
  | arrayoflist (x::xs)  = x $$ arrayoflist (xs);

fun find p (Lf, sub) = []
  | find p ((Br(v,t1,t2)), sub)  =
    if p(v)
    then sub::(find (p) ((t1), (sub*2)))@(find (p) ((t2), (sub*2+1)))
				   else (find p (t1, sub*2))@(find p (t2, sub*2+1));
				   
