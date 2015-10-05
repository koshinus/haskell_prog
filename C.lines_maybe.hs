{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, IncoherentInstances #-}

-----------------------------------------------------
-- TVector type
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

-----------------------------------------------------
-- Multiplication a vector by a number
class DoubleSideMul t1 t2 where 
  (><) :: t1 -> t2 -> TVector

instance (Real t) => DoubleSideMul t TVector where
  a >< (Vector x y) = Vector (b*x) (b*y)
    where b = (fromRational (toRational a)) :: Double

instance (Real t) => DoubleSideMul TVector t where
  (Vector x y) >< a = Vector (b*x) (b*y)
    where b = (fromRational (toRational a)) :: Double

-----------------------------------------------------
-- Line type
-- data TLine = Line Double Double Double   -- Coefficients A, B, C in Ax+By+C = 0

data TLine = Line 
  {
  cA :: Double,
  cB :: Double,
  cC :: Double   
  } deriving(Eq,Show)

-- Build a middle perpendicular of a serment 
middlePerp :: TVector -> TVector -> TLine
middlePerp (Vector x1 y1) (Vector x2 y2) = Line a b c where
  xm = (x1 + x2) / 2  
  ym = (y1 + y2) / 2  
  a = x2 - x1
  b = y2 - y1
  c = -(a*xm + b*ym)

-- Cross two lines accurately 
crossLines :: TLine -> TLine -> Maybe TVector
crossLines l1 l2 = 
  let
    d = cA l1 * cB l2 - cB l1 * cA l2
    d1 = -(cC l1 * cB l2 - cB l1 * cC l2)
    d2 = -(cA l1 * cC l2 - cC l1 * cA l2)
    x = d1 / d
    y = d2 / d
  in
    if d == 0
      then Nothing
      else Just (Vector x y)
   

