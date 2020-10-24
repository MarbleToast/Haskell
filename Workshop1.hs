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
set [] = False
set (x : xs) = contains xs x || set xs

largest :: [Int] -> Int
largest [a] = a
largest (x : xs)
  | (maximum xs) > x = maximum xs
  | otherwise = x

zipped :: [a] -> [s] -> [(a, s)]
zipped [x] [y] = [(x, y)]
zipped (x : xs) (y : ys) = [(x, y)] ++ zipped xs ys

insert :: Int -> [Int] -> [Int]
insert i [x]
  | i >= x = [x, i]
  | otherwise = [i, x]
insert i (x : xs)
  | i >= x = [x] ++ insert i xs
  | i < x = i : x : xs

main :: IO ()
main = putStrLn (show (insert (-1) [1]))