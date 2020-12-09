module Day3 (part1, part2, solvePart1, solvePart2) where

type Area = [[Char]]

type Route = [(Int, Int)]

type Dimension = (Int, Int)

type Direction = (Int, Int)

route :: Dimension -> Direction -> Route
route (width, height) (x, y) = takeWhile (\(a, b) -> b < height) $ zipWith (\a b -> (a `mod` width, b)) [x, 2 * x ..] [y, 2 * y ..]

dimension :: Area -> Dimension
dimension area = (length . head $ area, length area)

treesOnSlope :: Area -> Direction -> Int
treesOnSlope area = length . filter (== '#') . map (\(x, y) -> (area !! y) !! x) . route (dimension area)

solvePart1 :: Area -> Int
solvePart1 area = treesOnSlope area (3, 1)

solvePart2 :: Area -> Int
solvePart2 area = product . map (treesOnSlope area) $ [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]

part1 :: String -> Int
part1 = solvePart1 . lines

part2 :: String -> Int
part2 = solvePart2 . lines