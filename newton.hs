newton n = map (\ x -> (fact n)`div`((fact x)*(fact (n-x)))) [0..n]
fact x = product [1..x]