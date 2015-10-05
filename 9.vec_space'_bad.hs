class DoubleSideMul t1 t2 where 
  (><) :: t1 -> t2 -> TVector

data TVector = Vector Double Double    -- Two coordinates
   deriving(Eq,Ord) 

instance (Real t) => DoubleSideMul t TVector where
  a >< (Vector x y) = Vector (b*x) (b*y)
    where b = (fromRational (toRational a)) :: Double

instance (Real t) => DoubleSideMul TVector t where
  (Vector x y) >< a = Vector (b*x) (b*y)
    where b = (fromRational (toRational a)) :: Double





















{-

  class C t1 t2 where ...

  instance context1 => C Int a     where ...  -- (A)
  instance context2 => C a   Bool  where ...  -- (B)
  instance context3 => C Int [a]   where ...  -- (C)
  instance context4 => C Int [Int] where ...  -- (D)

1) имеетс€ возможное перекрытие вариантов (A) и (B), например, 
   при попытке объ€вить C Int Bool.

2) ѕусть есть объ€вление
   f :: [b] -> [b]
   f x = ...
и права€ часть объ€влени€ f приводит к ограничению — Int [b]. » компил€тор не может выбрать между реализаци€ми (C) и (D). 

-}













