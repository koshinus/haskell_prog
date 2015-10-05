-- groups.google.com/group/cs_2year

-- 1_2_3_4_5_6_7_8_9

data TFigure =
  Rectangle Double Double Double Double |
         -- x1 y1 x2 y2 |
  Circle Double Double Double
         -- x0 y0 r
  
square :: TFigure -> Double
square (Rectagle x1 y1 x2 y2) =
  abs (x1 - x2) * abs (y1 - y2)
square (Circle _ _ r) = 3.141592 * r^2

data Boolean = True' | False'
not True' = False'
not False' = True'

data TSuit = Spades | Clubs | Diamonds | Hearts
  deriving (Show,Eq,Ord)
  
-- Jack | Queen | King | Ace  

data TValue = V2 | V3 | ... | V10 | Jack | ... | Ace
  deriving (Show,Eq,Ord)

f :: (a,a) -> (a,a) -> Bool
f (v1,v2) (v1,v2) = ... -- error