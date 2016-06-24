fun itpow(x:real,res:real,n:int) = if n=1 then res else 
if n mod 2 = 0 then itpow(x,res*res, n div 2)
else itpow(x, x*res*res,n div 2);
 
fun pow (x:real, n:int) = itpow (x, x, n);

fun power (x:int, res:int, n:int) = 
    if n=1 then res
    else 
        if n mod 2 =0 
        then power (x,res*res, n div 2)
        else power (x, x*res*res, n div 2);

infix ^^;

fun (a ^^ b) = power (a, a, b);


