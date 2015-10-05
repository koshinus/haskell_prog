gcd' :: Int -> Int -> Int
gcd' x y = 
  if  mod x y == 0
    then y
	  else gcd' y (mod x y)