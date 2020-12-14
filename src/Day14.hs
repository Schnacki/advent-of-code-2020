module Day14 (part1, part2, Input (..), solvePart1) where

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
  _ <- string "mask = "
  mask <- M.some alphaNumChar
  _ <- optional $ char '\n'
  return $ Mask mask

parseMem :: Parser Input
parseMem = do
  _ <- string "mem["
  addr <- L.decimal
  _ <- string "] = "
  value <- L.decimal
  _ <- optional $ char '\n'
  return $ Mem addr value

inputParser = M.many $ choice [parseMask, parseMem]

parseInput :: String -> [Input]
parseInput input = case M.parse inputParser "" input of
  Left _ -> []
  Right a -> a

applyMask :: String -> Int -> Int
applyMask mask value = value .&. andMask .|. orMask
  where
    andMask :: Int
    andMask = complement . foldl (\b a -> 2 * b + if a == '0' then 1 else 0) 0 $ mask
    orMask = foldl (\b a -> 2 * b + if a == '1' then 1 else 0) 0 mask

solvePart1 :: [Input] -> Int
solvePart1 input = solve input Map.empty ""
  where
    solve [] map _ = Map.foldr (+) 0 map
    solve ((Mask newMask) : xs) map _ = solve xs map newMask
    solve ((Mem addr value) : xs) map mask = solve xs (Map.alter (\_ -> Just (applyMask mask value)) addr map) mask

part1 :: String -> Int
part1 = solvePart1 . parseInput

part2 :: String -> Int
part2 str = 0