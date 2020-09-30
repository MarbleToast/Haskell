len :: [a] -> Int
len [] = 0
len (x : xs) =
  1 + len (xs)

contains :: [String] -> String -> Bool
contains (x : xs) val
  | x == val = True
  | xs == [] = False
  | otherwise = contains xs val

set :: [String] -> Bool

set (x : xs)

main :: IO ()
main = putStrLn (show (set ["To", "Be", "Or"]))