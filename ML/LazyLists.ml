datatype 'a seq = Nil
		| Cons of 'a * (unit->'a seq);

fun seqFrom i = Cons(i, fn () => seqFrom (i+1));

exception empty;

fun Hd (Cons(x, _ ) ) = x
  | Hd Nil = raise empty;

fun Tl (Cons(_, f)) = f()
  | Tl Nil  = Nil;

val nat = seqFrom 1;

fun tk Nil _ = []
  | tk _ 0 = []
  | tk (Cons(x,xs)) n =
    x::(tk (xs()) (n-1));


fun filterSeq p Nil = Nil
  | filterSeq p (Cons(x, xs)) =
    if p x then
	Cons (x,( fn() => filterSeq p (xs())))
    else
	filterSeq p (xs());

val evens = filterSeq (fn n => n mod 2 =0) nat;


fun nth _ Nil = raise empty
  | nth 0 (Cons(x,_)) = x
  | nth n (Cons(_, xs)) = nth (n-1) xs();

(* The following is a keen demonstration of the Sieve of Eratosthenes*)

(* Will find a sequence of Primes*)

fun sieve Nil = Nil
  | sieve Cons(x, xs)  =
    let
	fun sift a ns = filterSeq (fn () n => n mod a <> 0) ns
    in
	Cons(x, fn () => sift x xs)
    end;

val primes = sieve (seqFrom 2);



					 
