func:: Ord a => [a] -> [a] -> [a]
func l1 l2 = iter l1 l2 []
	where
		iter (x:xs) (y:ys) zs 
								|x==y = iter xs ys (x:y:zs)
								|x>y = iter (x:xs) ys (y:zs)
								|otherwise = iter xs (y:ys) (x:zs)
						
		iter [] l2 res = (reverse res)++l2
		iter l1 [] res = (reverse res)++l1