{-class  Monad m  where
    (>>=)       :: forall a b. m a -> (a -> m b) -> m b
    (>>)        :: forall a b. m a -> m b -> m b
    return      :: a -> m a-}

import Control.Monad.Writer
	
gcd1 a 0 = a
gcd1 a b = gcd1 b (a `mod` b)

gcd2 :: Int->Int-> Writer [(Int,Int)] Int
gcd2 a b = do
  tell [(a,b)]
  if b == 0
    then return a
	else gcd2 b (a `mod` b)
	
algSum0 :: Num a => [a] -> [a] -> [[a]]
algSum0 lst1 lst2 = map (\a -> map (\b -> a+b) lst2) lst1

algSum1 :: Num a => [a] -> [a] -> [a]
algSum1 lst1 lst2 = 
    concat $ map (\a -> 
	    concat $ map (\b -> [a+b]) lst2) lst1
		
algSum2 :: Num a => [a] -> [a] -> [a] -> [a]
algSum2 lst1 lst2 lst3= 
    concat $ map (\a -> 
	    concat $ map (\b -> 
		    concat $ map (\c ->[a+b+c]) lst3) lst2) lst1
			
algSum3 lst1 lst2 lst3 =
    a <- lst1
	b <- lst2
	c <- lst3
	return (a+b+c)
	
algSum4 lst1 lst2 lst3 = [a+b+c |a <- lst1, b <- lst2, c <- lst3]