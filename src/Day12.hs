module Day12 (part1, part2, solvePart1, solvePart2, Action (..)) where

data Action = NORTH Int | SOUTH Int | EAST Int | WEST Int | LEFT Int | RIGHT Int | FORWARD Int deriving (Show)

instance Read Action where
  readsPrec _ (action : number) = case action of
    'N' -> [(NORTH $ read number, "")]
    'S' -> [(SOUTH $ read number, "")]
    'E' -> [(EAST $ read number, "")]
    'W' -> [(WEST $ read number, "")]
    'L' -> [(LEFT $ read number, "")]
    'R' -> [(RIGHT $ read number, "")]
    'F' -> [(FORWARD $ read number, "")]
    _ -> []

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
solvePart1 input =
  let (Ship (x, y) _) = steps (Ship (0, 0) 90) input
   in abs x + abs y

part1 :: String -> Int
part1 = solvePart1 . fmap read . lines

data Ship2 = Ship2 (Int, Int) (Int, Int) deriving (Show)

steps2 :: Ship2 -> [Action] -> Ship2
steps2 = foldl step
  where
    step (Ship2 (x, y) (wx, wy)) action = case action of
      NORTH v -> Ship2 (x, y) (wx - v, wy)
      SOUTH v -> Ship2 (x, y) (wx + v, wy)
      EAST v -> Ship2 (x, y) (wx, wy + v)
      WEST v -> Ship2 (x, y) (wx, wy - v)
      LEFT v -> case v of
        0 -> Ship2 (x, y) (wx, wy)
        90 -> Ship2 (x, y) (- wy, wx)
        180 -> Ship2 (x, y) (- wx, - wy)
        270 -> Ship2 (x, y) (wy, - wx)
      RIGHT v -> case v of
        0 -> Ship2 (x, y) (wx, wy)
        90 -> Ship2 (x, y) (wy, - wx)
        180 -> Ship2 (x, y) (- wx, - wy)
        270 -> Ship2 (x, y) (- wy, wx)
      FORWARD v -> Ship2 (x + v * wx, y + v * wy) (wx, wy)

solvePart2 :: [Action] -> Int
solvePart2 input =
  let (Ship2 (x, y) _) = steps2 (Ship2 (0, 0) (-1, 10)) input
   in abs x + abs y

part2 :: String -> Int
part2 = solvePart2 . fmap read . lines