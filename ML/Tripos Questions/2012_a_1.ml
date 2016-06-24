datatype 'a tree = Lf
		 | Br of 'a *'a tree * 'a tree;

fun update Lf (k,v) = Br((k,v),Lf,Lf)
  | update  (Br(k1,v1), t1, t2) (k,v)  =
    if k < k1
    then Br((k1,v1), update t1 (k,v), t2)
    else Br((k1,v1), t1, update t2 (k,v) );

fun union Lf t = t
  | union (Br(lbl), t1, t2) t  =
    union (t1 union (t2 update t lbl));


fun takeSlice (x,y) Lf = Lf
  | takeSlice (x,y) (Br((k,v), left,right)) =
    if k>x andalso k<y
			 then (Br((k,v), takeSlice (x,y) left, takeSlice (x,y) right) 
