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

1) ������� ��������� ���������� ��������� (A) � (B), ��������, 
   ��� ������� �������� C Int Bool.

2) ����� ���� ����������
   f :: [b] -> [b]
   f x = ...
� ������ ����� ���������� f �������� � ����������� � Int [b]. � ���������� �� ����� ������� ����� ������������ (C) � (D). 

-}













