import System.IO

main :: IO ()
main = do
--putStr - оставляет курсор на месте
--putStrLn - переносит курсор на следущую строчку
  inData <- lines $ readFile "input.txt"

  let res = sum $ zipWith (*) (head inData) (last inData)

  writeFile "output.txt" (show res)

main1 = do 
		putStr "Hello," 
		putStrLn "world!"
		
main2 = do 
	putStr "Enter your name: "
	a <- getLine 
	putStrLn "Hello," ++ a
	
main3 = do
	putStr "Enter your name: "
	a <- getLine
	let
	   num = read a
	   res = num*2
	putStrLn $ "The doubled line is " ++ show res

sirak :: Int -> IO ()
sirak a = do
	if a == 1
	    then  return()
		else if even a
		    then do
			    print a
				sirak (a `div` 2)
			else do
			    print a
				sirak (3*a+1)
				
sirak1 a = 
	if even a
		then a `div` 2
		else 3*a+1

main4 = do
	putStr "Enter a0: "
	inData <- getLine
	let
	   a = read inData :: Int
	   lst = takeWhile (/= 1) $ iterate sirak1 a
	mapM print lst
	print 1
	{-sirak a
	print 1-}
	
main5 = do
	putStr "Enter a0 d n: "
	inData <- getLine
	let
		a0:d:n:_ = map read $ words inData :: [Int]
		lst = take n $ iterate (+ d) a0
		res = unwords $ map show lst
		res1 = unlines $ map show lst
	putStrLn res
	putStrLn res1
	{-print a0
	print d
	print n-}