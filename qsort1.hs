qsort1 f [] = []
qsort1 f (x:l) = (qsort1 f [y | y <- l, (f y x)])++[x]++(qsort1 f [y | y <- l, not(f y x)])