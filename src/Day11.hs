module Day11 (part1, part2, parseInput, (!?), (<!!>), update, solvePart1) where

import Data.Maybe (mapMaybe)

type Position = (Int, Int)

(!?) :: [a] -> Int -> Maybe a
list !? index = if index < 0 || index >= length list then Nothing else Just $ list !! index

(<!!>) :: [[Grid]] -> Position -> Maybe Grid
field <!!> (x, y) = field !? y >>= (!? x)

adjacent :: Position -> [Position]
adjacent (x, y) = [(x + x', y + y') | x' <- [-1, 0, 1], y' <- [-1, 0, 1], x' /= 0 || y' /= 0]

data Grid = Floor | Empty | Occupied deriving (Show, Eq)

update :: Position -> [[Grid]] -> Grid
update (x, y) field = case field <!!> (x, y) of
  Just Empty -> if notElem Occupied . mapMaybe (field <!!>) . adjacent $ (x, y) then Occupied else Empty
  Just Occupied -> if (>= 4) . length . filter (== Occupied) . mapMaybe (field <!!>) . adjacent $ (x, y) then Empty else Occupied
  Just g -> g

step :: [[Grid]] -> [[Grid]]
step grid = map (\y -> map (\x -> update (x,y) grid) [0.. length (head grid) - 1]) [0.. length grid - 1]

solvePart1 :: [[Grid]] -> Int
solvePart1 grid =
  let newGrid = step grid
   in if newGrid == grid then sum . map (length . filter (== Occupied)) $ grid else solvePart1 newGrid

parseInput :: String -> [[Grid]]
parseInput = fmap (fmap s) . lines
  where
    s '.' = Floor
    s 'L' = Empty
    s '#' = Occupied

part1 :: String -> Int
part1 = solvePart1 . parseInput

part2 :: String -> Int
part2 _ = 0