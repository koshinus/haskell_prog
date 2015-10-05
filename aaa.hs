aaa x = case x of 
             {1 ->  "A";
             2 ->  "B";
             3 ->  "C"}
			 
			 
			 


						
data CSuit = Spades | Clubs | Diamonds | Hearts
 deriving (Show, Eq, Ord)
data CValue = V2 | V3 | V4 | V5 | V6 | V7 | V8 | V9 | V10 | Jack | Queene | King | Ace
 deriving (Eq, Ord)
data CCard = Card CSuit CValue
 deriving (Eq, Ord)
 
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