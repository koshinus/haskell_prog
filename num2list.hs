num2list num = if num == 0 then [0] else iter num []
  where
    iter n l = 
	  if n == 0
	    then l
		else iter (n `div` 10) ((n `mod` 10):l)

num2list1 0 = []
num2list1 num = (num `mod` 10):(num2list1 (num `div` 10))