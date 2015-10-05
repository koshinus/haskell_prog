data TVector = Vector (x::Double, y::Double)    -- Two coordinates
   deriving(Show,Eq,Ord)
-- Num нельзя выводить
x::TVector -> Double
y::TVector -> Double

v1 = Vector 1.5 2.5
v2 = Vector {x = 1.5, y = 2.5}
v2 = Vector {y = 2.5, x = 1.5}

print:: TVector->String
print v = "(" ++ show (x v) ++ ";" ++ show (y v) ++ ")"

class Vector a where 
 f1:: ... -> ...
 f2:: ... -> ...
 ...
class (Num a, Eq a, Ord a) => Vector a where 
 (.+) :: a -> a -> a
 opposite :: a -> a
 (.-) ::
 opposite v = zeroVec 
 
instance Vector TIntVector  where
  (.+) (IntVector v1) (IntVector v2) = IntVector (zipWith (+) v1 v2)
  
v1 = IntVector [1,2]
v2 = IntVector [-1,3]