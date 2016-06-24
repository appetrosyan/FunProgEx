fun dayCount   "February"= 29
 |  dayCount   "October" = 31
 |  dayCount   "December"= 31
 |  dayCount   "January" = 31
 |  dayCount   "June"    = 31
 |  dayCount   "July"    = 31
 |  dayCount   _         = 30;
 fun validDay (d:int, lim:int)  = d>0 andalso d<lim;      
fun isDate (d:int, m:string) = validDay (d, dayCount m);

