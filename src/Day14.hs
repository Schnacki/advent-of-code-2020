{-# LANGUAGE TupleSections #-}

module Day14 (part1, part2, Input (..), solvePart1, solvePart2,BitOperation(..),convertMask) where

import Data.Bits
import Data.Maybe( mapMaybe)
import qualified Data.Map as Map
import Data.Void
import Text.Megaparsec as M
import Text.Megaparsec.Char as C
import Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

data Input = Mask String | Mem Int Int deriving (Show)

parseMask :: Parser Input
parseMask = do
  string "mask = "
  mask <- M.some alphaNumChar
  optional $ char '\n'
  return $ Mask mask

parseMem :: Parser Input
parseMem = do
  string "mem["
  addr <- L.decimal
  string "] = "
  value <- L.decimal
  optional $ char '\n'
  return $ Mem addr value

parseInput :: String -> [Input]
parseInput input = case M.parse (M.many $ choice [parseMask, parseMem]) "" input of
  Left _ -> []
  Right a -> a

type Memory = Map.Map Int Int

data BitOperation = Set | Clear | Floating deriving (Show, Eq)

type BitModification = (Int, BitOperation)

convertMask :: [(Char,BitOperation)] -> String -> [BitModification]
convertMask lk = mapMaybe (\(i,b) -> (i,) <$> (lookup b lk)) . zip [35, 34 ..]

applyMask :: [BitModification] -> Int -> [Int]
applyMask [] value = [value]
applyMask (op : ops) value = case op of
      (i, Set) -> map (`setBit` i) (applyMask ops value)
      (i, Clear) -> map (`clearBit` i) (applyMask ops value)
      (i, Floating) -> applyMask ops value >>= (\v -> [clearBit v i, setBit v i])

solvePart1 :: [Input] -> Int
solvePart1 input = solve input Map.empty ""
  where
    solve [] mem _ = Map.foldr (+) 0 mem
    solve ((Mask newMask) : xs) mem _ = solve xs mem newMask
    solve ((Mem addr value) : xs) mem mask = solve xs (updateMemory mem addr value mask) mask
    updateMemory mem addr value mask = Map.insert addr (head (applyMask (convertMask [('1',Set),('0',Clear)] mask) value)) mem

part1 :: String -> Int
part1 = solvePart1 . parseInput

solvePart2 :: [Input] -> Int
solvePart2 input = solve input Map.empty ""
  where
    solve [] mem _ = Map.foldr (+) 0 mem
    solve ((Mask newMask) : xs) mem _ = solve xs mem newMask
    solve ((Mem addr value) : xs) mem mask = solve xs (updateMemory mem addr value mask) mask
    updateMemory mem addr value mask = foldr (\a map -> Map.insert a value map) mem (applyMask (convertMask [('1',Set),('X',Floating)] mask) addr)

part2 :: String -> Int
part2 = solvePart2 . parseInput