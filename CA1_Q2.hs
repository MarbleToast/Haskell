{-
The late John Conway was a mathematician who became known outside the world of
mathematics for his invention of the Game of Life. The game involves configuring
a cellular automaton and then observing how it evolves according to three rules.
The cells are organised as a two-dimensional grid, and are either live or dead.

The three rules of evolution are:
    • Any live cell with two or three live neighbours survives.
    • Any dead cell with three live neighbours becomes alive.
    • All other live cells die, and all other dead cells stay dead.

The next generation is created by applying the rules simultaneously to every cell
in the current generation, so that births and deaths occur simultaneously.

One famous pattern of cells, the glider, shows how the rules of evolution are applied.
-}

-- PREDEFINED DECLARATIONS
import Control.Monad (liftM2)
import Data.List (nub)

type Point = (Int, Int)

glider :: [Point]
glider = [(0, 2), (1, 3), (2, 1), (2, 2), (2, 3)]

-- 2.1
-- turns 2D string lists into a 2D grid of strings
pretty :: [[String]] -> String
pretty = concatMap unlines

-- 2.2
-- creates 2D string list demonstrating points on a 2D grid being alive/dead
{--
    for each point set:
        for each row:
            for each column:
                for each point:
                    mark as a '#' if alive and '.' if dead
--}
visualisation :: Int -> Int -> [[Point]] -> [[String]]
visualisation width height =
  map (\livingSet -> [(map (isAlive livingSet) [(x, y) | x <- [0 .. width - 1]]) | y <- [0 .. height - 1]])
  where
    -- if the point is a member of the combined set of all points, return '#', else return '.'
    isAlive pointSet (a, b)
      | (a, b) `elem` pointSet = '#'
      | otherwise = '.'

-- 2.3
-- iterate through each generation of points, filtering out cells that do not survive from each neighbour of a living point (removing dupes)
evolution :: [Point] -> [[Point]]
evolution = iterate (nub . liftM2 filter livingCells allNeighbours)
  where
    -- get surrounding points for a given point
    neighbours :: Point -> [Point]
    neighbours (x, y) = [(nx, ny) | nx <- [x -1 .. x + 1], ny <- [y -1 .. y + 1]]

    -- gets all points neighboured by a list of living points
    allNeighbours :: [Point] -> [Point]
    allNeighbours = concatMap neighbours

    -- returns amount of neightbours needed for a point to live (either survive or revive)
    neighboursNeeded :: [Point] -> Point -> [Int]
    neighboursNeeded allPoints point
      | point `elem` allPoints = [3, 4]
      | otherwise = [3]

    -- does the length of the list of living neighbours around a point match what is needed for a point to revive/survive
    livingCells :: [Point] -> Point -> Bool
    livingCells allPoints point = length (filter (`elem` allPoints) (neighbours point)) `elem` (neighboursNeeded allPoints point)

-- MAIN FUNCTION AS DESCRIBED
main :: IO ()
main = putStrLn (pretty (take 8 (visualisation 5 5 (evolution glider))))