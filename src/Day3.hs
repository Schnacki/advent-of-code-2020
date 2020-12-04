module Day3(part1, part2, solvePart1, solvePart2) where

type Area = [[Char]]
type Route = [(Int, Int)]

route :: (Int, Int) -> (Int, Int) -> Route
route (width, height) (x, y) = takeWhile (\(a, b) -> b < height) $ zipWith (\a b -> (a `mod` width, b)) [x, 2 * x ..] [y, 2 * y ..]

dimension :: Area -> (Int, Int)
dimension area =  (length $ head area, length area)

solveSlope :: Area -> (Int, Int) -> Int
solveSlope area slope = length . filter (== '#') . map (\(x, y) -> (area !! y) !! x) $ route (dimension area) slope

solvePart1 :: Area -> Int
solvePart1 area = solveSlope area (3, 1)

solvePart2 :: Area -> Int
solvePart2 area = product . map (solveSlope area) $ [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]

part1 :: FilePath -> IO ()
part1 file = readFile file >>= (print . solvePart1 . lines)

part2 :: FilePath -> IO ()
part2 file = readFile file >>= (print . solvePart2 . lines)