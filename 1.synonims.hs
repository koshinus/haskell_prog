type TCartesian = (Rational,Rational)
type TPolar = (Rational,Rational)

test :: Bool
test =
  let 
    a = (1,2)::TCartesian
    b = (1,2)::TPolar
  in
    a == b

test1 :: TCartesian -> TPolar -> Bool
test1 a b = a == b
