{-
A puzzle requires one to find six-digit numbers where:
1.  all digits are different;
2.  alternate digits are even and odd, or odd and even;
3.  alternate digits differ by more than two;
4.  the first and middle pairs of digits form numbers that are both multiples of the last.

One solution is 496307, and another is 692703.
-}

-- PREDEFINED DECLARATIONS
import Data.List (nub, tails)

-- 1.1
-- if the length of a non-duped list is still 6, there were no duplicates
rule1 :: (Int, Int, Int, Int, Int, Int) -> Bool
rule1 (a, b, c, d, e, f) = length (nub [a, b, c, d, e, f]) == 6

-- 1.2
-- if each successive pair of digits are of differing parity, return true, else false
rule2 :: (Int, Int, Int, Int, Int, Int) -> Bool
rule2 (a, b, c, d, e, f) = and [int1 `mod` 2 /= int2 `mod` 2 | (int1 : int2 : _) <- tails [a, b, c, d, e, f]]

-- 1.3
-- if the distance between each successive pair is more than 2, return true, else false
rule3 :: (Int, Int, Int, Int, Int, Int) -> Bool
rule3 (a, b, c, d, e, f) = and [abs (int1 - int2) > 2 | (int1 : int2 : _) <- tails [a, b, c, d, e, f]]

-- 1.4
-- if the first and second pair are mulitples of the third pair (i.e mod third pair == 0), return true, else false
rule4 :: (Int, Int, Int, Int, Int, Int) -> Bool
rule4 (a, b, c, d, e, f) = concatInts a b `mod` thirdPair == 0 && concatInts c d `mod` thirdPair == 0
  where
    -- the sum of the first of the pair being the tens and the second being the ones
    concatInts a b = a * 10 + b
    thirdPair = concatInts e f

-- 1.5
-- generates all possible 6 digit numbers via list comprehension
-- also could be done via replicateM but I think this is more readable
possibles :: [(Int, Int, Int, Int, Int, Int)]
possibles = [(a, b, c, d, e, f) | a <- [0 .. 9], b <- [0 .. 9], c <- [0 .. 9], d <- [0 .. 9], e <- [0 .. 9], f <- [0 .. 9]]

-- 1.6
-- if all four rules eval true for a number, return true, else false
isSolution :: (Int, Int, Int, Int, Int, Int) -> Bool
isSolution x = and [rule1 x, rule2 x, rule3 x, rule4 x]

-- MAIN FUNCTION AS DESCRIBED
main :: IO ()
main = putStrLn (show (filter isSolution possibles))