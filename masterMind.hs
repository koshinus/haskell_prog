import System.IO
import System.Random
import Data.List
{-Один из игроков загадывает последовательность цифр длины n (в ней возможны повторения цифр). Другой поочередно предлагает свои версии этого набора, 
получая от загадывающего подсказки: сколько цифр стоит на своих местах, а сколько присутствует в числе, но не на своих местах. Например, для n = 4:

0430 <- загадано
----
4103 3п (3 цифры присутствуют, но не на своих местах). 
Заметим, что ноль сопоставляется только с одним 
нулем из исходного числа)
4230 2м 1п (2 цифры стоят на своих местах, 1 присуствует, 
но не на месте; ноль связывается с наилучшим 
из двух нулей)
0430 4м - угадали! 

Задача: реализовать на Хаскелле загадывающего игрока:
- запросить у пользователя длину n загадываемого набора;
- организовать загадывание случайного набора из n цифр;
- организовать ввод версии угадывающего пользователя;
- организовать анализ этой версии и адекватную выдачу информации о правильности загадывания; в случае угадывания результата остановить работу.-}
--numToList n = show n reverse $ map (`mod` 10) (takeWhile (> 0) (iterate (`div` 10) n))

randomn n = sequence [randomRIO ('0', '9') | _ <- [1..n]]

check x y 
		| x==y = [y]
		| otherwise = []

bulls lst1 lst2 = concat $ zipWith check lst1 lst2
					
cows lst1 lst2	= lst1 \\ (lst1 \\ lst2)

control lst1 lst2 = let 
						p = length (bulls lst1 lst2)
						q = length (cows lst1 lst2) 
						  in (p,q-p)
					
masterMind = do 
	putStrLn $ "Напишите скольки разрядное число вы хотите отгадать."
	num <- readLn::(IO Integer)
	putStrLn $ "Напишите свою первую догадку."
	a <- readLn::(IO Integer)
	b <- randomn num
	putStrLn $ show b
	resFunc b (show a)

iterFunc n m = let p = control n m in
							if snd p == 0 && fst p == 0
								then do
									putStrLn $ show (snd p)++"п"
									a <- getLine
									resFunc n (show a)
								else do
									putStrLn $ show (fst p)++"м "++show (snd p)++"п"
									a <- getLine
									resFunc n (show a)

resFunc n m = if n == "0"++m || n == m 
					then putStrLn $ show (length n) ++ "м - угадали"
					else if (length n)-(length m) == 1 	
							then iterFunc n ("0"++m)
							else iterFunc n m