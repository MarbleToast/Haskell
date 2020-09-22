{-
    Commands to execute:
    > ghci
    > :load Main
    > main
-}

rev :: [a] -> [a] -- type signature
rev [] = []
rev (x : xs) = rev xs ++ [x]

double :: Int -> Int
double x = x * 2

concatenateIntAndString :: Int -> String -> String
concatenateIntAndString int string = "The result is " ++ show int ++ ", also: " ++ string

main :: IO ()
main = do
  string <- getLine
  putStrLn (concatenateIntAndString 13 string)