applist :: [a] -> [a] -> [a]
applist x y = x ++ y

applist_foldr :: [a] -> [a] -> [a]
applist_foldr x y = foldr (++) y [x]

revlist_sorta :: [a] -> [a]
revlist_sorta = reverse

revlist :: [a] -> [a]
revlist [] = []
revlist (x : xs) = applist (revlist (xs)) [x]

revlist_foldr :: [a] -> [a]
revlist_foldr x = foldr (applist . revlist) [] [x]

vowels :: String -> String
vowels [] = []
vowels (x : xs)
  | x `elem` "aeiou" = x : vowels (xs)
  | otherwise = vowels (xs)

vowels_filter :: String -> String
vowels_filter = filter (\a -> a `elem` "aeiou")

concatList :: [[a]] -> [a]
concatList [] = []
concatList (x : xs) = x ++ (concatList xs)

concatList_foldr :: [[a]] -> [a]
concatList_foldr [] = []
concatList_foldr x = foldr (++) [] x

pam :: [a -> b] -> a -> [b]
pam [] _ = []
pam (x : xs) v = x v : pam xs v
