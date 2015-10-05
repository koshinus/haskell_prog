med l = reverse $ foldl check [] l

check (x:xs) 
			|mod x 2 == 0 = (x:(x:xs))
			|otherwise = (x:xs)
			
--(\ res x -> if (mod x 2 == 0) then (x:(x:res)) else (x:res))