combine :: (Ord a) => [a] -> [a] -> [a]
combine l1 l2 = iter l1 l2 []
  where 
    iter l ll lll =
	  if (null l) 
	    then (reverse lll)++ll
	    else if (null ll)
		  then (reverse lll)++l
		  else if (head l)<=(head ll)
		    then iter (tail l) ll ((head l):lll)
			else iter l (tail ll) ((head ll):lll)

{--help ls1 [] = ls1
help [] ls2 = ls2
help ls1 ls2 = 
  if (head ls1)<=(head ls2)
    then (head ls1)
	else (head ls2)
combine1 l1 l2 = foldl (\ l -> (help l1 l2):l) [] --}