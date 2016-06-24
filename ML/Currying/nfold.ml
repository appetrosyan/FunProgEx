fun  nfoldr f x 0 = x
  |  nfoldr f x n = nfoldr f (f(x)) (n-1);

