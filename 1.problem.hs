{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, IncoherentInstances #-}

import Data.Maybe

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

lenVec :: TVector -> Double
lenVec (Vector x y) = sqrt (x**2 + y**2)

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
middlePerp :: TVector -> TVector -> Maybe TLine
middlePerp p1 p2 
 | p1 == p2  = Nothing
 | otherwise = Line a b c where
     Vector xm ym = 0.5 >< (p1 + p2)
     Vector a b = p2 - p1
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
   
-----------------------------------------------------
-- Circle type
data TCircle = Circle
  {
  center :: TVector,
  radius :: Double
  }
  deriving(Eq,Show)

circumcircle :: TVector -> TVector -> TVector -> Maybe TCircle
circumcircle p1 p2 p3 = 
  let
    l1 = middlePerp p1 p2
    l2 = middlePerp p1 p3
    c = crossLines l1 l2
    r = lenVec (fromJust c - p1)
  in
    if (isNothing l1) || (isNothing l2)
      then Nothing
      else if isNothing c
        then Nothing
        else Just (Circle (fromJust c) r)