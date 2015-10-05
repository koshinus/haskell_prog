isPrime :: Int -> Bool
isPrime n = iter n 2
  where 
    q = floor . sqrt . fromIntegral $ n
    iter n m = 
	  if m > q
	    then True
		else if mod n m == 0
		  then False
		  else iter n (m+1)