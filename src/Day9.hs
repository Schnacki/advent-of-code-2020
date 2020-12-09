module Day9 (part1, part2, solvePart1, tuples) where

tuples :: [a] -> [(a, a)]
tuples [] = []
tuples (x:xs) = (fmap ((,) x) xs) ++ tuples xs

isSum :: Int -> [Int] -> Bool
isSum result = any (\(a, b) -> a + b == result) . tuples

solvePart1 :: Int -> [Int] -> Int
solvePart1 pLength list = if isSum res a then solvePart1 pLength (tail list) else res
  where (a,res:_) = splitAt pLength list

parseInput :: String -> [Int]
parseInput = fmap read . lines

part1 :: FilePath -> IO ()
part1 file = print . solvePart1 32 . parseInput=<< readFile file

part2 :: FilePath -> IO ()
part2 file = readFile file >> print "TODO"
