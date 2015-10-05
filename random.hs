import System.IO
import System.Random
import Data.List

{-main1 = do
	--g <- getStdGen
	let
		g = mkStdGen
		lst = take 10 $ randomRs (0,1) g::Int
	print lst

main = do
	inData <- readFile "input.txt"
	let
	   nums = map read $ lines inData :: [Int]
	   res = sum nums
	writeFile "output.txt" $ show res-}
	
{-ran = do
	putStr "Enter your range: "
	inData <- getLine
	n <- randomRIO (1,1000) :: IO Int
	g <- getStdGen
	let 
		a:b:_ = map read $ lines inData::[Double] 
		lst = take n $ randomR g (a,b)
	writeFile "binary.txt" $ unwords $ map show lst-}
	
sortFunc a b | a >= b = LT
			 |otherwise = GT

fromSortTo = do
	inData <- readFile "input.txt"
	let 
	   lst = map read $ words inData :: [Int]
	   res = sortBy sortFunc lst
	writeFile "output.txt" $ unwords $ map show res
{-guess = do
	putStr "Make a number from 1 to 1000000 "
	inData <--}