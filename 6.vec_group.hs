data TVector = Vector Double Double deriving(Eq,Ord,Num) 

instance Show TVector where
  show (Vector x y) = "(" ++ show x ++ ";" ++ show y ++ ")"

zeroVec :: TVector
zeroVec = Vector 0 0

