fun divide n d = 
    let 
    
    fun prescale p d = if n>d 
    then prescale (p*2) (d*2) 
    else (p,d) 
    val (p,d) = prescale 1 d (*left shift Loop*)

    fun mainloop n d p r = 
        if p=0 then r
        else 
            let val (n, r) = if n>=d 
            then (n-d, r+p) 
            else (n,r)
            in mainloop n (d div 2) (p div 2) r 
            end
    in mainloop n d p 0
    end;


fun mpx1 (x,y,c) = 
    if x=0 then c else 
        let val (x', n) = (x div 2, x mod 2) 
        val y' = y*2
        val c' = case n of 
        0 => (c)
       |1 => (c+y) 
        in mpx1 (x',y',c')
        end;

fun mpx2 (x,y,c,carry) = 
    if x=0 andalso carry =0 then c else
        let val (x', n) = (x div 2, x mod 2 +carry) 
        val y' = y*2
        val (carry', c') = case n of
        0 => (0,c)
       |1 => (0,c+y)
       |2 => (1,c) 
        in mpx2 (x',y',c',carry')
        end;

fun booth (x,y,c,carry) = 
    if x=0 andalso carry=0 then c else
        let val (x',n) = (x div 4, x mod 4+ carry)
        val y' = y*4
        val (carry', c') = case n of 
        0 => (0,c) 
       |1 => (0,c+y)
       |2 => (0, c+2*y)
       |3 => (1, c-y)
       |4 => (1,c)
        in booth (x',y',c',carry')
        end;

    


