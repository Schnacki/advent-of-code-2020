{-# LANGUAGE TupleSections #-}

module Day14 (part1, part2, Input (..), solvePart1, solvePart2) where

import Data.Bits
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import Data.Void(Void)
import Text.Megaparsec (choice, many, optional, Parsec, parse, some)
import Text.Megaparsec.Char (alphaNumChar, char, string)
import Text.Megaparsec.Char.Lexer (decimal)

type Parser = Parsec Void String

data Input = Mask String | Mem Int Int deriving (Show)

parseMask :: Parser Input
parseMask = do
  string "mask = "
  mask <- some alphaNumChar
  optional $ char '\n'
  return $ Mask mask

parseMem :: Parser Input
parseMem = do
  string "mem["
  addr <- decimal
  string "] = "
  value <- decimal
  optional $ char '\n'
  return $ Mem addr value

parseInput :: String -> [Input]
parseInput input = case parse (many $ choice [parseMask, parseMem]) "" input of
  Left _ -> []
  Right a -> a

type Memory = M.Map Int Int

data BitOperation = Set | Clear | Floating deriving (Show, Eq)

type BitModification = (Int, BitOperation)

convertMask :: [(Char, BitOperation)] -> String -> [BitModification]
convertMask lk = mapMaybe (\(i, b) -> (i,) <$> lookup b lk) . zip [35, 34 ..]

applyModification :: [BitModification] -> Int -> [Int]
applyModification [] value = [value]
applyModification (op : ops) value = case op of
  (i, Set) -> applyModification ops value >>= (\v -> [v `setBit` i])
  (i, Clear) -> applyModification ops value >>= (\v -> [v `clearBit` i])
  (i, Floating) -> applyModification ops value >>= (\v -> [v `clearBit` i, v `setBit` i])

solvePart1 :: [Input] -> Int
solvePart1 input = solve input M.empty []
  where
    solve [] mem _ = M.foldr (+) 0 mem
    solve ((Mask mask) : xs) mem _ = solve xs mem (convertMask [('1', Set), ('0', Clear)] mask)
    solve ((Mem addr value) : xs) mem mod = solve xs (updateMemory mem addr value mod) mod
    updateMemory mem addr value mod = M.insert addr ((head . applyModification mod) value) mem

part1 :: String -> Int
part1 = solvePart1 . parseInput

solvePart2 :: [Input] -> Int
solvePart2 input = solve input M.empty []
  where
    solve [] mem _ = M.foldr (+) 0 mem
    solve ((Mask mask) : xs) mem _ = solve xs mem (convertMask [('1', Set), ('X', Floating)] mask)
    solve ((Mem addr value) : xs) mem mod = solve xs (updateMemory mem addr value mod) mod
    updateMemory mem addr value mod = foldr (\a map -> M.insert a value map) mem (applyModification mod addr)

part2 :: String -> Int
part2 = solvePart2 . parseInput