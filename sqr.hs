roots :: (Float, Float, Float) -> Float
roots (a,b,c) = if d < 0 then error "No roots" else max x1 x2
  where x1 = (-b + sd) / (2 * a)
	x2 = (-b - sd) / (2 * a)
	d  = b * b - 4 * a * c
	sd = sqrt d