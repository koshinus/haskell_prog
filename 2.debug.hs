data TDebug a = Debug (a,String)

f' :: a -> TDebug b
f' x = Debug (f x,"calling f with x = " ++ 
                  (show x) ++ "\n")

g' ::  b -> TDebug c
g' x = Debug (g x,"calling g with x = " ++ 
                  (show x) ++ "\n")

				  
nextFunc :: TDebug a -> (a -> TDebug b) -> TDebug b
nextFunc (Debug (a,mess)) f = 
  (
   f a, 
   mess ++ "calling " ++ (show f) ++ 
     " with x = " ++ (show x) ++ "\n"
  )

startDebug :: a -> TDebug a
startDebug v = (v,"")