rowsum n = iter 1 n 0
  where 
    iter k n s = 
	  if (k>n)
	    then s
		else  iter (k + 1) n (s + (1/(k^2)))

rowsum1 0 =	0	
rowsum1 n = 1/(n^2) + rowsum1(n-1)