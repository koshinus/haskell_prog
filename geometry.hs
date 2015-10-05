data Figure = Rectangle Double Double Double Double | Circle Double Double Double
 deriving (Show,Ord,Eq)
data IFigure = Intersection [Figure] | Union [Figure] | Single Figure
 deriving (Show,Ord,Eq)

makeVector (x1,y1) (x2,y2) = (x2-x1,y2-y1)

vectorInRectangle (Rectangle x1 y1 x2 y2) = zipWith makeVector [(x1,y1),(x1,y2),(x2,y2),(x2,y1)] [(x1,y2),(x2,y2),(x2,y1),(x1,y1)]

vecp (x1,y1) (x2,y2) = x1*y2 - x2*y1

isRectangle (Rectangle _ _ _ _) = True
isRectangle _ = False

inRectangle point (Rectangle x1 y1 x2 y2) = 
         and $ zipWith (\ x y -> (vecp x y)>=0) (vectorInRectangle (Rectangle x1 y1 x2 y2)) (map (\ x -> makeVector point x) [(x1,y1),(x1,y2),(x2,y2),(x2,y1)])

inCircle point (Circle x y r) = r^2 >= ((fst point)-x)^2 + ((snd point)-y)^2

inFigureIntersection point l = and $ map (\ x -> if isRectangle x 
                                                    then inRectangle point x 
													else inCircle point x) l
													
inFigureUnion point l = or $ map (\ x -> if isRectangle x then inRectangle point x else inCircle point x) l

contains :: (Double,Double) -> IFigure -> Bool
contains point figure = case figure of 
                            {(Single (Circle x y r))->inCircle point (Circle x y r);
							(Single (Rectangle x1 y1 x2 y2))->inRectangle point (Rectangle x1 y1 x2 y2);
                            (Intersection l)->inFigureIntersection point l;
                            (Union l)->inFigureUnion point l}
							 
isRectangular figure = and $ map isRectangle figure