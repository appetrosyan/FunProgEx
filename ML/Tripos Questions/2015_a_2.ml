datatype 'a stream = Null
	           | Cons of 'a * (unit-> 'a stream);
exception empty;
fun head Null = raise empty
  | head (Cons(a, _)) = a;

fun tail Null = raise empty
  | tail (Cons(_,f)) = f();

fun constant x = Cons (x, fn () => x);
fun fromk (k,n) = Cons(k, fn () => fromk (k+n,n));
val ints = fromk (1,1);

fun streamtolist (Null, _) = []
  | streamtolist (_, 0) = [] 
  | streamtolist ((Cons(h,t)), k) = h::streamtolist(t(), k-1);

fun take (Null, _) = raise empty
  | take (strm, 0) = head strm
  | take (strm, k) = take(tail strm, k-1);

fun diag (Cons(strm, t)) =
  let
      fun getal (Cons(strm, t), k) =
	Cons(take (strm,k), fn () => getal(t, k+1))
  in
      getal(Cons(strm,t), 0)
  end;

fun flatten (Cons(Cons(x,xs),xss)) =
  Cons(x, flatten(Cons(xs(),xss)));

(* Will just extract everything from the first stream*)

fun interleave (Cons(x,xs)) (Cons(y,ys)) =
  Cons(x, fn () => Cons(y, interleave (xs,ys)));

fun flatten2 (Cons(Cons(x,xs),xss)) =
  Cons(x, interleave (xs, head xss));

(* Will outut everything from the first row and Column but there must be a better way of doing this*)

fun tabulate _ = Null;
