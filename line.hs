import Data.List
import Data.Maybe

data TLine = Line { a :: Double, b :: Double, c :: Double }
 deriving (Show,Eq,Ord)

data Point = Point Double Double
 deriving (Show,Eq,Ord)

intersect' (Line a1 b1 c1) (Line a2 b2 c2) = let 
							d = a1*b2-b1*a2 
							d1 = (-c1)*b2-b1*(-c2) 
							d2 = (-c2)*a1-a2*(-c1) in
									  if d == 0 then Nothing
										else Just (Point (d1/d) (d2/d) )
							
{-intersect2 l = iter l [] l [] 0
  where
	iter l1 l2 l3 res x =
		if l1 == [] then res
			else if l2 == [] then iter (tail l1) (take x l3 ++ drop (x+1) l3) l3 res x+1
			else iter (x:l1) [] l3 ((map (x `intersect`) l2):res) x-}
			
intersect2' l = nub $ map fromJust [b | b <- (concat $ [map (a `intersect'`) (filter (/=a) l) | a <- l]), isJust b]