list2num l = iter l 0
  where
    iter l1 n = 
	  if (null l1)
	    then n
		else iter (tail l1) (n*10 + (head l1))

--list2num1 l = foldl (\ n -> (n*10 + (head l))) 0 (tail l)