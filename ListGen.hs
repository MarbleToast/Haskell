createList :: Int -> Int -> Int -> [Int]
createList from to step
  | from <= to = from : createList (from + step) to step
  | otherwise = []

main :: IO ()
main = putStrLn (show (createList 1 100 2))