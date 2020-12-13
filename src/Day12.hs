module Day12 (part1, part2, solvePart1, solvePart2, Action (..)) where

data Action = NORTH Int | SOUTH Int | EAST Int | WEST Int | LEFT Int | RIGHT Int | FORWARD Int deriving (Show)

type Position = (Int, Int)

instance Read Action where
  readsPrec _ (action : n) = case action of
    'N' -> [(NORTH $ read n, "")]
    'S' -> [(SOUTH $ read n, "")]
    'E' -> [(EAST $ read n, "")]
    'W' -> [(WEST $ read n, "")]
    'L' -> [(LEFT $ read n, "")]
    'R' -> [(RIGHT $ read n, "")]
    'F' -> [(FORWARD $ read n, "")]
    _  -> []

manhattan :: Position -> Int
manhattan (x, y) = abs x + abs y

data Ship = Ship {pos :: Position, direction :: Int} deriving (Show)

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

data Ship2 = Ship2 {pos' :: Position, wayPoint :: Position} deriving (Show)

rotate :: Position -> Int -> Position
rotate (wx, wy) angle = ([wx,wy,-wx,-wy] !! index, [wy,-wx,-wy,wx] !! index)
  where index = div angle 90

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