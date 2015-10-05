trnum1 n = tail $ scanl (+) 0 [1..n]

toBynNumSyst n = {-reverse (map (\ x -> x - (x `div` 2)*2)-} (takeWhile (> 0) (iterate (\ x -> x `div` 2) n))
resultList n = map toBynNumSyst [1..n]