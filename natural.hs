data Natural  = Unit | Next Natural
  deriving(Show,Eq,Ord)
{-instance Show Natural where
  show Unit = "1"
  show Next (Unit) = "2"  -}

{-printf:: Natural -> String
printf Unit = "1"
printf n = "n"-}

next :: Natural -> Natural
next n = Next n

instance Num Natural where
  (+) n Unit  = Next n
  (+) n (Next m) = Next (n + m)
  negate _ = undefined
  (*) n Unit = n
  (*) n (Next m) = m + (m * n)
  abs _ = undefined
  signum _ = Unit
  fromInteger _ = undefined