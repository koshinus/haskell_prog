import Control.Monad.Writer

recip' :: Double -> Writer [String] Double
recip' x = do
  tell ["recip res: " ++ show (recip x)]
  return $ recip x
  
asin' :: Double -> Writer [String] Double
asin' x = do
  tell ["asin res: " ++ show (asin x)]
  return $ asin x
  
log' ::  Double -> Writer [String] Double
log' x = do
  tell ["log res: " ++ show (log x)]
  return $ log x
  
sqrt' :: Double -> Writer [String] Double
sqrt' x = do
  {
  tell ["sqrt res: " ++ show (sqrt x)];
  return $ sqrt x
  }

f1 x = 
  runWriter $ return x >>= recip' >>= asin' >>= 
  log' >>= sqrt'

{-  
f2 x = 
  runWriter $ do
    y <- recip' x
	z <- asin' y
	w <- log' z
	sqrt' w
-}