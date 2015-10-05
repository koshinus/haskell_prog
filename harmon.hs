--сумма гармонического ряда
import Data.Ratio
-- % - из 2 целых формирует рациональное
harmon:: Rational -> Rational
harmon a = head $ dropWhile (< a) $ scanl (+) 0 $ map (1 %) $ [1..]

-- кол-во членов ряда, сумма которых превосходит а
harmon1:: Rational -> Rational
harmon1 a = length $ takeWhile (<= a) $ tail $ scanl (+) 0 $ map (1 %) $ [1..]