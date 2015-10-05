import Control.Monad.Writer

gcd1 :: Int -> Int -> Int
gcd1 a 0 = a
gcd1 a b = gcd1 b (a `mod` b)

gcd2 :: Int -> Int -> Writer [(Int,Int)] Int
gcd2 a b = do
  tell [(a,b)]
  if b == 0
    then return a
	else gcd2 b (a `mod` b)