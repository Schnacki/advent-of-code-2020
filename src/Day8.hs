module Day8 (part1, part2, solvePart1, solvePart2, Operation (NOP, ACC, JMP), Computation (Computation)) where

import Data.Maybe (mapMaybe)

data Operation = NOP Int | ACC Int | JMP Int deriving (Show, Eq)

data Computation = Computation {seen :: [Int], opIndex :: Int, acc :: Int, ops :: [Operation]}

inLoop :: Computation -> Bool
inLoop (Computation seen index _ _) = index `elem` seen

parseInput :: String -> Computation
parseInput = Computation [] 0 0 . map parseLine . lines
  where
    parseLine ('n' : 'o' : 'p' : ' ' : number) = NOP . read . filter (/= '+') $ number
    parseLine ('a' : 'c' : 'c' : ' ' : number) = ACC . read . filter (/= '+') $ number
    parseLine ('j' : 'm' : 'p' : ' ' : number) = JMP . read . filter (/= '+') $ number

performOperation :: Computation -> Computation
performOperation (Computation seen index val ops) = case ops !! index of
  NOP _ -> Computation (index : seen) (index + 1) val ops
  ACC a -> Computation (index : seen) (index + 1) (val + a) ops
  JMP j -> Computation (index : seen) (index + j) val ops

solvePart1 :: Computation -> Int
solvePart1 computation
  | inLoop computation = acc computation
  | otherwise = solvePart1 . performOperation $ computation

corruptComputation :: Computation -> [Computation]
corruptComputation (Computation seen index val ops) = fmap (Computation seen index val) (flipOps [] ops)
  where
    mayFlip (ACC _) = False
    mayFlip _ = True
    flipOp (ACC a) = ACC a
    flipOp (NOP n) = JMP n
    flipOp (JMP j) = NOP j
    flipOps h [] = []
    flipOps h (t : ts) = let ops = flipOps (h ++ [t]) ts in if mayFlip t then (h ++ flipOp t : ts) : ops else ops

solvePart2 :: Computation -> Int
solvePart2 = head . mapMaybe solve . corruptComputation
  where
    solve comp
      | inLoop comp = Nothing
      | opIndex comp >= (length . ops) comp = Just $ acc comp
      | otherwise = solve . performOperation $ comp

part1 :: FilePath -> IO ()
part1 file = print . solvePart1 . parseInput =<< readFile file

part2 :: FilePath -> IO ()
part2 file = print . solvePart2 . parseInput =<< readFile file