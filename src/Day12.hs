module Day12 (part1, part2, solvePart1, solvePart2, Action (..)) where

data Action = NORTH Int | SOUTH Int | EAST Int | WEST Int | LEFT Int | RIGHT Int | FORWARD Int deriving (Show)

instance Read Action where
  readsPrec _ ('N' : n) = [(NORTH $ read n, "")]
  readsPrec _ ('S' : n) = [(SOUTH $ read n, "")]
  readsPrec _ ('E' : n) = [(EAST $ read n, "")]
  readsPrec _ ('W' : n) = [(WEST $ read n, "")]
  readsPrec _ ('L' : n) = [(LEFT $ read n, "")]
  readsPrec _ ('R' : n) = [(RIGHT $ read n, "")]
  readsPrec _ ('F' : n) = [(FORWARD $ read n, "")]
  readsPrec _ xs = []

manhattan :: (Int, Int) -> Int
manhattan (x, y) = abs x + abs y

data Ship = Ship {pos :: (Int, Int), direction :: Int} deriving (Show)

steps :: Ship -> [Action] -> Ship
steps = foldl step
  where
    step (Ship (x, y) dir) action = case action of
      NORTH v -> Ship (x - v, y) dir
      SOUTH v -> Ship (x + v, y) dir
      EAST v -> Ship (x, y + v) dir
      WEST v -> Ship (x, y - v) dir
      LEFT v -> Ship (x, y) ((dir - v) `mod` 360)
      RIGHT v -> Ship (x, y) ((dir + v) `mod` 360)
      FORWARD v -> case dir of
        0 -> Ship (x - v, y) dir
        90 -> Ship (x, y + v) dir
        180 -> Ship (x + v, y) dir
        270 -> Ship (x, y - v) dir

solvePart1 :: [Action] -> Int
solvePart1 = manhattan . pos . steps (Ship (0, 0) 90)

part1 :: String -> Int
part1 = solvePart1 . fmap read . lines

data Ship2 = Ship2 {pos' :: (Int, Int), wayPoint :: (Int, Int)} deriving (Show)

rotate :: (Int, Int) -> Int -> (Int, Int)
rotate (wx, wy) 0 = (wx, wy)
rotate (wx, wy) 90 = (wy, - wx)
rotate (wx, wy) 180 = (- wx, - wy)
rotate (wx, wy) 270 = (- wy, wx)

steps2 :: Ship2 -> [Action] -> Ship2
steps2 = foldl step
  where
    step (Ship2 (x, y) (wx, wy)) action = case action of
      NORTH v -> Ship2 (x, y) (wx - v, wy)
      SOUTH v -> Ship2 (x, y) (wx + v, wy)
      EAST v -> Ship2 (x, y) (wx, wy + v)
      WEST v -> Ship2 (x, y) (wx, wy - v)
      LEFT v -> Ship2 (x, y) (rotate (wx, wy) (negate v `mod` 360))
      RIGHT v -> Ship2 (x, y) (rotate (wx, wy) v)
      FORWARD v -> Ship2 (x + v * wx, y + v * wy) (wx, wy)

solvePart2 :: [Action] -> Int
solvePart2 = manhattan . pos' . steps2 (Ship2 (0, 0) (-1, 10))

part2 :: String -> Int
part2 = solvePart2 . fmap read . lines