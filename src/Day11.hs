module Day11 (part1, part2, parseInput, (!?), (<!!>), update, solvePart1) where

import Data.Maybe (mapMaybe)

data Grid = Floor | Empty | Occupied deriving (Show, Eq)

type Area = [[Grid]]

type Position = (Int, Int)

(!?) :: [a] -> Int -> Maybe a
list !? index = if index < 0 || index >= length list then Nothing else Just $ list !! index

(<!!>) :: Area -> Position -> Maybe Grid
field <!!> (x, y) = field !? y >>= (!? x)

adjacentFields :: Area -> Position -> [Grid]
adjacentFields field = mapMaybe (field <!!>) . adjacent
  where adjacent (x, y) = [(x + x', y + y') | x' <- [-1, 0, 1], y' <- [-1, 0, 1], x' /= 0 || y' /= 0]


update :: Position -> Area -> Grid
update pos field = case field <!!> pos of
  Just Empty -> if notElem Occupied . adjacentFields field $ pos then Occupied else Empty
  Just Occupied -> if (>= 4) . length . filter (== Occupied) . adjacentFields field $ pos then Empty else Occupied
  Just g -> g

step :: Area -> Area
step grid = map (\y -> map (\x -> update (x,y) grid) [0.. length (head grid) - 1]) [0.. length grid - 1]

solvePart1 :: Area -> Int
solvePart1 grid =
  let newGrid = step grid
   in if newGrid == grid then sum . map (length . filter (== Occupied)) $ grid else solvePart1 newGrid

parseInput :: String -> Area
parseInput = fmap (fmap s) . lines
  where
    s '.' = Floor
    s 'L' = Empty
    s '#' = Occupied

part1 :: String -> Int
part1 = solvePart1 . parseInput

part2 :: String -> Int
part2 _ = 0