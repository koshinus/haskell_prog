--fix f = f (fix f)
sinn n = iterate sin 1 !! n