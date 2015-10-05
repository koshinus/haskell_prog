data Complex = Complex Double Double
 deriving (Eq,Ord)
instance Show Complex where 
 show (Complex a b) = show a ++ "+" ++ show b ++ "i"
 
instance Num Complex where
  (+) (Complex a b) (Complex c d) = (Complex (a+c) (b+d))
  negate (Complex a b) = (Complex (-a) (-b))
  (*) (Complex a b) (Complex c d) = (Complex (a*c-b*d) (a*d+b*c))
  abs:: Complex -> Double
  abs (Complex a b) = sqrt $ (a^2+b^2)
  signum (Complex a 0) = signum a
  signum (Complex a b) = undefined
  fromInteger a = (Complex (fromInteger a) 0)