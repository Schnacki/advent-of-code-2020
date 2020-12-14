module Day14 (part1, part2, Input (..), solvePart1, solvePart2, parseInput,BitOperation(..),applyMask', parseMask') where

import Data.Bits
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

applyMask :: String -> Int -> Int
applyMask mask value = value .&. andMask .|. orMask
  where
    andMask = complement . foldl (\b a -> 2 * b + if a == '0' then 1 else 0) 0 $ mask
    orMask = foldl (\b a -> 2 * b + if a == '1' then 1 else 0) 0 mask

solvePart1 :: [Input] -> Int
solvePart1 input = solve input Map.empty ""
  where
    solve [] mem _ = Map.foldr (+) 0 mem
    solve ((Mask newMask) : xs) mem _ = solve xs mem newMask
    solve ((Mem addr value) : xs) mem mask = solve xs (Map.insert addr (applyMask mask value) mem) mask

part1 :: String -> Int
part1 = solvePart1 . parseInput


data BitOperation = Set | Clear | Floating deriving Show
type BitModification = (Int,BitOperation)

parseMask' :: String -> [BitModification]
parseMask' = map (\(b,i) -> if i == '1' then (b,Set) else (b,Floating)) . filter (\(_,i) -> i /= '0') . zip [35, 34 ..0]

applyMask' :: [BitModification] -> Int -> [Int]
applyMask' [] value = [value]
applyMask' ((i,Set):ops) value = map (\v -> setBit v i) (applyMask' ops value)
applyMask' ((i,Clear):ops) value = map (\v -> clearBit v i) (applyMask' ops value)
applyMask' ((i,Floating):ops) value= (applyMask' ops value) >>= ((\v -> [clearBit v i,setBit v i]))


solvePart2 :: [Input] -> Int
solvePart2 input = solve input Map.empty ""
  where
    solve [] mem _ = Map.foldr (+) 0 mem
    solve ((Mask newMask) : xs) mem _ = solve xs mem newMask
    solve ((Mem addr value) : xs) mem mask = solve xs (foldr (\a map -> Map.insert a value map) mem  (applyMask' (parseMask' mask) addr)) mask

part2 :: String -> Int
part2 = solvePart2 . parseInput