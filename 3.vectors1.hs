data TVector = Vector Double Double    -- Two coordinates

plus :: TVector -> TVector -> TVector
plus (Vector x1 y1) (Vector x2 y2) = Vector (x1+x2) (y1+y2)

scalar :: TVector -> TVector -> Double
scalar (Vector x1 y1) (Vector x2 y2) = x1*x2+y1*y2