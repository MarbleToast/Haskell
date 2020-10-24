append :: [t] -> [t] -> [t]
append (x : xs) ys = x : append ys xs
append [] ys = ys

mystery :: (Num b, Enum b) => b -> b
mystery n = foldr (*) 1 [1 .. n]