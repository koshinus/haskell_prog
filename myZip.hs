myZip l1 [] = []
myZip [] l2 = []
myZip (x:xs) (y:ys) = (x,y):(myZip xs ys)