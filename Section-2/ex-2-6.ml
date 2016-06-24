fun comesBefore (h1:int, m1:int, am1:string) (h2:int, m2:int, am2:string) = 
        let val ht1 = if am1 = "AM" then h1 else h1+12
            val ht2 = if am2 = "AM" then h2 else h2+12
        in 
           if ht1 = ht2 then (if m1 < m2 then m1 else m2)
           else if ht1<ht2 then ht1
           else ht2
       end;
