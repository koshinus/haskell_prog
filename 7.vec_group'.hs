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

f x = if x==0 then Error else if abs y>1 then Error else if x <= 0 then Error else if w <0 then Error else res
where y=recip' x
      z= asin'    fromCorrect $ y 
	  w= log'     fromCorrect $ z 
	  res = sqrt' fromCorrect $ w

passComp :: MaybeError Double -> (Double -> MaybeError Double) -> MaybeError Double
passComp q f = if q == Error then Error else f $ fromCorrect q

f2 x = passComp sqrt' x $ passComp log' $ asin' $ passComp recip' $ toCorrect x

data MaybeError a = Correct a | Error 
 deriving (Show,Eq)

fromCorrect (Correct val) = val

recip' :: Double -> MaybeError Double
recip' x = if x == 0 then Error else Correct (recip x)

asin' :: Double -> MaybeError Double
asin' x = if abs x > 1 then Error else Correct (asin x)

log' :: Double -> MaybeError Double
log' x = if x <= 0 then Error else Correct (log x)

sqrt' :: Double -> MaybeError Double
sqrt' x = if x < 0 then Error else Correct (sqrt x)

f1 x = return x >>= recip' >>= asin' >>= log' >>= sqrt'
f3 x = Just x >>= recip' >>= asin' >>= log' >>= sqrt'
{-f4 x = 
    runWriter $ do
	    y <- recip' x
		z <- asin' y
		w <- log' z
		sqrt' w-}
		
f5 x = 
    runWriter $ do
	    \x -> recip' x >>=
		(\y -> asin' y >>=
		(\z -> log' z >>=
		(\w -> sqrt' w)))