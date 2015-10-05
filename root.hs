root::(Double->Double) -> Double -> Double -> Double -> Double
root f a b eps =
	if (abs(b)-abs(a))<=eps
      then (abs(b)-abs(a))
      else if (f(a)*f((abs(b)-abs(a))/2))==0
	    then if f(a)==0 
		        then a
				else (abs(b)-abs(a))/2
		  else if f(a)*f((abs(b)-abs(a))/2)>0 
		    then root f a ((abs(b)-abs(a))/2) eps
		    else root f ((abs(b)-abs(a))/2) b eps 