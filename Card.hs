data CSuit = Spades | Clubs | Diamonds | Hearts
 deriving (Show, Eq, Ord)
data CValue = V2 | V3 | V4 | V5 | V6 | V7 | V8 | V9 | V10 | Jack | Queene | King | Ace
 deriving (Show, Eq, Ord)
data CCard = Card CSuit CValue
 deriving (Show, Eq, Ord)

instance Show CCard where
    show (Card s v) = "(" ++ show s ++ ";" ++ show v ++ ")"

isMinor (Card _ v) = not $ v == Jack || v == Queene || v == King || v == Ace

sameSuit (Card s1 _) (Card s2 _) = s1 == s2

beats (Card s1 v1) (Card s2 v2) = sameSuit (Card s1 v1) (Card s2 v2) && v1 > v2

beats2 (Card s1 v1) (Card s2 v2) trump = if s1 == s2 
                                            then beats (Card s1 v1) (Card s2 v2)
											else if s1 == trump then True else False
											
beatsList l (Card s v) trump = foldl (\ res x -> if (beats2 x (Card s v) trump) then (x:res) else res) [] l

blackJack l = let p = foldl (\ (res1,res2) (Card x v) -> if v == Ace then (res1+1,res2) else (res1, cost (Card x v) + res2)) (0,0) l
					in map (+ (snd p)) (reslist (fst p))
					
cost (Card _ v) = case v of 
					{V2->2; 
					V3->3; 
					V4->4; 
					V5->5; 
					V6->6; 
					V7->7; 
					V8->8; 
					V9->9; 
					V10->10; 
					Jack->10; 
					Queene->10; 
					King->10}
					
reslist m = map (\ x -> (m-x)*1+x*11) [0..m]