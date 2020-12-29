module Day24 (part1, part2) where

import qualified Data.Map as M
import Control.Applicative((<|>))

type Position = (Int, Int)

data Tile =  White | Black deriving (Show, Eq)

flipTile :: Tile -> Tile
flipTile White = Black
flipTile Black = White

generatePositions :: [String] -> [Position]
generatePositions = fmap (go (0,0))
  where
    go :: Position -> String -> Position
    go pos [] = pos
    go (x, y) ('e':is) = go (x + 1, y) is
    go (x, y) ('s':'e':is) = go (x + 1, y - 1) is
    go (x, y) ('s':'w':is) = go (x, y - 1) is
    go (x, y) ('w':is) = go (x - 1, y) is
    go (x, y) ('n':'w':is) = go (x - 1, y + 1) is
    go (x, y) ('n':'e':is) = go (x, y + 1) is

solvePart1 :: [Position] -> Int
solvePart1 = length . M.filter (== Black) . foldl (\map p -> M.alter (\tile -> flipTile <$> tile <|> Just Black) p map) M.empty

part1 :: String -> Int
part1 = solvePart1 . generatePositions . lines

part2 :: String -> Int
part2 _ = 0