fun revapp ([],ys)=ys
    | revapp (x::xs, ys) = revapp(xs,x::ys);
