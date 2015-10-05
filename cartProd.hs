cartProd1 l1 l2 = [(a,b) | a <- l1, b <- l2]

cartProd2 :: [a] -> [b] -> [(a,b)]
cartProd2 l1 l2 = iter l1 l2 l2 []
  where 
    iter l1 l2 l3 res =
	    if null l1 then res
	       else if null l2 then iter (tail l1) l3 l3 res
		        else iter l1 (tail l2) l3 ((head l1,head l2):res)
				
cartProd3 :: [a] -> [b] -> [(a,b)]
cartProd3 l1 l2 = do 
	a <- l1	
	b <- l2
	return (a,b)