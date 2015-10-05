data TCartesian2D = Cartesian2D Double Double    -- Two coordinates
data TPolar2D = Polar2D Double Double            -- Angle and distance

test :: Bool
test =
  let 
    a = TCartesian2D 1 2
    b = TPolar2D 1 2
  in
    a == b

test1 :: TCartesian2D -> TPolar2D -> Bool
test1 a b = a == b
