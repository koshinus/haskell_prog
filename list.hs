algSum0 :: Num a => [a] -> [a] -> [[a]]
algSum0 lst1 lst2 =
  map (\a -> map (\b -> a+b) lst2 ) lst1
  
algSum1 :: Num a => [a] -> [a] -> [a]
algSum1 lst1 lst2 =
  concat $ map (\a -> 
    concat $ map (\b -> [a+b]) lst2 ) lst1  
	
algSum2 :: Num a => [a] -> [a] -> [a] -> [a]
algSum2 lst1 lst2 lst3 =
  concat $ map (\a -> 
    concat $ map (\b -> 
	  concat $ map (\c -> [a+b+c]) lst3 )
	  lst2 ) lst1  	
	  
algSum3 :: Num a => [a] -> [a] -> [a] -> [a]
algSum3 lst1 lst2 lst3 = do
  a <- lst1
  b <- lst2
  c <- lst3
  return (a+b+c)
	  
algSum4 :: Num a => [a] -> [a] -> [a] -> [a]
algSum4 lst1 lst2 lst3 = [a+b+c |
  a <- lst1, b <- lst2, c <- lst3]