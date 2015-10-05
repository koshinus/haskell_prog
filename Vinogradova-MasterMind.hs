import System.Random
import Data.Char
import Data.List

import Control.Monad
import System.Exit

-- цифры присутствуют, но не на своих местах
presence :: (Eq a, Num b) => [a] -> [a] -> b
presence m e = foldl presence' 0 $ zip m e where
    presence' a (l, r) | l == r = a
                       | r `elem` m = a + 1
                       | otherwise = a

-- цифры стоят на своих местах
place :: (Eq a, Num b) => [a] -> [a] -> b
place m e = foldl place' 0 $ zip m e where
    place' a (l, r) | l == r = a + 1
                    | otherwise = a

winning :: IO a
winning = do
    putStrLn "You win!"
    exitSuccess

extractDigits :: [Char] -> [Char]
extractDigits = filter isDigit

print_suff :: Show a => a -> [Char] -> IO ()
print_suff v suff = putStrLn $ (show v) ++ suff

print_diff :: (Show a, Show a1) => a -> a1 -> IO ()
print_diff pre loc = do
    print_suff pre " присутствуют."
    print_suff loc " на месте."

solve :: t -> [Char] -> IO ()
solve n lst = do
    s <- getLine >>= return . extractDigits
    if lst == s then winning else print_diff (presence lst s) (place lst s)

randomDigitCharList :: (Enum t, Num t) => t -> IO [Char]
randomDigitCharList n = sequence [randomRIO ('0', '9') | _ <- [1..n]]

main :: IO ()
main = do
    putStrLn "Введите длину последовательности"
    n <- getLine >>= return.read
    lst <- randomDigitCharList n
    putStrLn (show lst)
    forever $ solve n lst