import Control.Monad.Writer
stringPrice str = 
				let p = foldl (\(res1,res2) x -> if x == 'e' then (res1+1,res2) else (res1,res2+(cost x))) (0,0) str 
				in
				map (+ (snd p)) (ePrice (fst p))

cost symbol = case symbol of 
					{'a' -> 1;
					 'b' -> 2;
					 'c' -> 10;
					 'd' -> 10}
					 
ePrice m = init $ map (\ x -> m+10*x) [0..(2*m+1)]

{-я понимаю, что это решение без использования монад, зато здесь есть идея: выписав различные варианты для 1 2 3 я понял, что 
количество различных вариантов стоимости е (при m - количестве е в строке) - 2m+1, причем каждая стоимость отличается от предыдущей ровно на 10
ePrice m = do
		a <- [0..m]
		b <- [0..m]
		c <- [0..m]
		return [a+b+c]-}
		
--ePrice1 m = [a+b*11+c*21 | a <- [0..m], b <- [0..(m-n)], c <- [0..(m-n-l)], [n | n <- [0..m]], [l | l <- [0..m-n]]]

gcd2 :: Int->Int-> Writer [(Int,Int)] Int
gcd2 a b = do
  tell [(a,b)]
  if b == 0
    then return a
	else gcd2 b (a `mod` b)