--решето эратосфена
erato:: Integral a => Int -> [a]
erato n = iter n [2..] []
  where
    iter n (x:l) res = 
	  if (length res) == n
	    then (reverse res)
		else iter n (primefilter x l) (x:res)

--выбор из списка тех элементов, которые не делятся на что-либо
primefilter:: Integral t => t -> [t] -> [t]
primefilter p l = [x | x <- l, (x `mod` p /= 0)]

--основная функция
fromIntToPrime:: Integral a => [a1] -> [a]
fromIntToPrime l = erato (length l)