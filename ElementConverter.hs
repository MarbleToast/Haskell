{-
    Commands to execute:
    > ghci
    > :load ElementConverter
    > main
-}

symbol :: String -> String
symbol "Beryllium" = "Be"
symbol "Hydrogen" = "H"
symbol "Helium" = "He"
symbol "Lithium" = "Li"
symbol s = [s !! 0, s !! 1]

main :: IO ()
main = putStrLn (symbol "Hydrogen")