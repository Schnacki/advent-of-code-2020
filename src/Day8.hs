module Day8 (part1, part2, solvePart1, solvePart2, Operation (NOP, ACC, JMP), Computation(Computation)) where

import Data.Maybe (mapMaybe)

data Operation = NOP Int | ACC Int | JMP Int deriving (Show, Eq)

data Computation = Computation [Int] Int Int [Operation]

flipOperations :: [Operation] -> [[Operation]]
flipOperations = flipOps []
  where
    mayFlip (ACC _) = False
    mayFlip _ = True
    flipOp (ACC a) = ACC a
    flipOp (NOP n) = JMP n
    flipOp (JMP j) = NOP j
    flipOps h [] = []
    flipOps h (t : ts) = let ops = f (h ++ [t]) ts in if mayFlip t then (h ++ flipOp t : ts) : ops else ops

parseInput :: String -> Computation
parseInput str = Computation [] 0 0 (map parseLine . lines $ str)
  where
    parseLine ('n' : 'o' : 'p' : ' ' : number) = NOP $ read (filter (/= '+') number)
    parseLine ('a' : 'c' : 'c' : ' ' : number) = ACC $ read (filter (/= '+') number)
    parseLine ('j' : 'm' : 'p' : ' ' : number) = JMP $ read (filter (/= '+') number)

solvePart1 :: Computation -> Int
solvePart1 (Computation seen index val ops)
  | index `elem` seen = val
  | otherwise = case ops !! index of
    NOP _ -> solvePart1 $ Computation (index : seen) (index + 1) val ops
    ACC a -> solvePart1 $ Computation (index : seen) (index + 1) (val + a) ops
    JMP j -> solvePart1 $ Computation (index : seen) (index + j) val ops

solvePart2 :: Computation -> Int
solvePart2 (Computation seen index val ops) = head . mapMaybe (solve seen index val) . flipOperations $ ops
  where
    solve seen index val ops
      | index `elem` seen = Nothing
      | index >= length ops = Just val
      | otherwise =
        case ops !! index of
          NOP n -> solve (index : seen) (index + 1) val ops
          ACC a -> solve (index : seen) (index + 1) (val + a) ops
          JMP j -> solve (index : seen) (index + j) val ops

part1 :: FilePath -> IO ()
part1 file = print . solvePart1 . parseInput =<< readFile file

part2 :: FilePath -> IO ()
part2 file = print . solvePart2 . parseInput =<< readFile file