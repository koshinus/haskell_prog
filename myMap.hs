myMap f [] = []
myMap f (x:list) = f$x:(myMap f list)

square x = x*x