module Day8 (part1, part2, solvePart1, solvePart2, Operation (NOP, ACC, JMP), Computation (Computation)) where

import Data.Maybe (mapMaybe)
import Data.Char (toUpper)

data Operation = NOP Int | ACC Int | JMP Int deriving (Show, Eq, Read)

data Computation = Computation {seen :: [Int], opIndex :: Int, acc :: Int, ops :: [Operation]}

parseInput :: String -> Computation
parseInput = Computation [] 0 0 . map read . lines . filter (/= '+') . map toUpper

inLoop :: Computation -> Bool
inLoop (Computation seen index _ _) = index `elem` seen

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

part1 :: String -> Int
part1 = solvePart1 . parseInput

part2 :: String -> Int
part2 = solvePart2 . parseInput