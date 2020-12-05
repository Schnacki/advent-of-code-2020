module Day5(part1, solvePart1) where

row :: String -> Int
row = foldl (\ b a -> 2 * b + if (a == 'B') then 1 else 0) 0

column :: String -> Int
column = foldl (\ b a -> 2 * b + if (a == 'R') then 1 else 0) 0

solvePart1 :: String -> Int
solvePart1 boardingPass = let (r, c) = splitAt 7 boardingPass
  in 8 * row r + column c

part1 :: FilePath -> IO ()
part1 file = (print . maximum . fmap solvePart1 . lines) =<< readFile file