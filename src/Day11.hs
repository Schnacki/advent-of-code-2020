module Day11 (part1, part2, parseInput, (<!!>), update, solvePart1) where

import Data.Maybe (mapMaybe)

data Grid = Floor Position | Empty Position | Occupied Position deriving (Show, Eq)

type Area = [[Grid]]

type Position = (Int, Int)

(<!!>) :: Area -> Position -> Maybe Grid
field <!!> (x, y) = field !? y >>= (!? x)
  where list !? index = if index < 0 || index >= length list then Nothing else Just . (list !!) $ index

adjacentFields :: Area -> Position -> [Grid]
adjacentFields field = mapMaybe (field <!!>) . adjacent
  where adjacent (x, y) = [(x + x', y + y') | x' <- [-1, 0, 1], y' <- [-1, 0, 1], not (x' == 0 && y' == 0)]

numbersOccupied = length . filter isOccupied
  where isOccupied (Occupied _) = True
        isOccupied _ = False

update :: Area -> Grid -> Grid
update area (Empty p) = if ( == 0 ) . numbersOccupied . adjacentFields area $ p then Occupied p else Empty p
update area (Occupied p) =  if (>= 4) . numbersOccupied . adjacentFields area $ p then Empty p else Occupied p
update _ grid = grid

solvePart1 :: Area -> Int
solvePart1 area =
  let newGrid = map (map (update area)) area
   in if newGrid == area then sum . map numbersOccupied $ area else solvePart1 newGrid

parseInput :: String -> Area
parseInput = fmap (\(r, row) -> fmap (\(c, grid) -> parse grid c r) (zip [0..] row)) . zip [0..] . lines
  where
    parse '.' c r = Floor (c,r)
    parse 'L' c r = Empty (c,r)
    parse '#' c r = Occupied (c,r)

part1 :: String -> Int
part1 = solvePart1 . parseInput

part2 :: String -> Int
part2 _ = 0