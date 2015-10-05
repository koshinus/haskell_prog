sumSquares1 l = foldl (+) 0 (map square l)
square x = x*x

sumSquares2 l = iter l 0
  where
    iter l s =
	  if (null l)
	    then s
		else iter (tail l) (s + square(head l))

sumSquares3 [] = 0
sumSquares3 (x:l) = square x + sumSquares3 l

{--lstToInt lst = sum $ map func $ zip lst [10^x | x <- (reverse [0..k])]
	where
		func = (\(a, b) -> a * b)
		k    = (length lst) - 1--}