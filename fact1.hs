import Control.Monad.State

fact0 n = factIter n 1
  where
    factIter 0 res = res
    factIter n res = factIter (n-1) (res*n)

fact1 :: Int -> State Int Int
fact1 0 = do
  res <- get
  return res
fact1 n = do
  res <- get
  put (n*res)
  fact1 (n-1)
  
fact2 :: Int -> State Int Int
fact2 0 = do
  res <- get
  return res
fact2 n = do
  modify (*n)
  fact2 (n-1)  
  
  
  
  
  
  
  