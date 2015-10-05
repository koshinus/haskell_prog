import Control.Monad.Writer

maxInList (y:ys) = foldM (\res x -> do 
						if res>=x	then do 
										tell ("->" ++ show res) 
										return res
									else do 
										tell ("->" ++ show x)	
										return x) y (y:ys)

maxInList1 l = iter l (head l)
   where 
	iter l res = do
		tell [show (max (head l) res) ++ "->"]
		if null (tail l) 
			then return res
			else iter (tail l) (max (head l) res)