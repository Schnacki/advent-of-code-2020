{-# LANGUAGE TupleSections #-}

module Day9 (part1, part2, solvePart1, solvePart2) where

tuples :: [a] -> [(a, a)]
tuples [] = []
tuples (x : xs) = ((x,) <$> xs) ++ tuples xs

isSum :: Int -> [Int] -> Bool
isSum result = any (\(a, b) -> a + b == result) . tuples

solvePart1 :: Int -> [Int] -> Int
solvePart1 pLength list =
  let (a, res : _) = splitAt pLength list
   in if isSum res a then solvePart1 pLength (tail list) else res

parseInput :: String -> [Int]
parseInput = fmap read . lines

part1 :: FilePath -> IO ()
part1 file = print . solvePart1 32 . parseInput =<< readFile file

sumTo :: Int -> (Int, Int) -> [Int] -> Maybe (Int, Int)
sumTo val (mx, mn) [] = if val == 0 then Just (mx, mn) else Nothing
sumTo val (mx, mn) (x:xs)
  | val == 0 = Just (mx, mn)
  | val < 0 = Nothing
  | otherwise = sumTo (val - x) (max x mx, min x mn) xs

solvePart2 :: Int -> [Int] -> Int
solvePart2 val (x:xs) = case sumTo val (x,x) (x:xs) of
  Just (mx, mn) -> mx + mn
  Nothing -> solvePart2 val xs

part2 :: FilePath -> IO ()
part2 file = print . solvePart2 258585477 . parseInput =<< readFile file