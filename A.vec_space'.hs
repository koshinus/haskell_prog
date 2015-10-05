{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, IncoherentInstances #-}


class DoubleSideMul t1 t2 where 
  (><) :: t1 -> t2 -> TVector


data TVector = Vector Double Double    -- Two coordinates
   deriving(Eq,Ord) 

instance Show TVector where
  show (Vector x y) = "(" ++ show x ++ ";" ++ show y ++ ")"

instance Num TVector where
  (+) (Vector x1 y1) (Vector x2 y2) = Vector (x1+x2) (y1+y2)
  negate (Vector x y) = Vector (-x) (-y)

-- (-) no definition, because a - b = a + (negate b) 

  (*) (Vector _ _) (Vector _ _) = undefined
  abs (Vector _ _) = undefined
  signum (Vector _ _) = undefined
  fromInteger _ = undefined

zeroVec :: TVector
zeroVec = Vector 0 0

instance (Real t) => DoubleSideMul t TVector where
  a >< (Vector x y) = Vector (b*x) (b*y)
    where b = (fromRational (toRational a)) :: Double

instance (Real t) => DoubleSideMul TVector t where
  (Vector x y) >< a = Vector (b*x) (b*y)
    where b = (fromRational (toRational a)) :: Double
