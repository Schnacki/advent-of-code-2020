module Day14 (part1, part2, parseInput, Input(..), solvePart1, applyMask) where

import Data.Bits
import qualified Data.Map as Map
import Data.Void
import Text.Megaparsec as M
import Text.Megaparsec.Char as C
import Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

data Input = Mask String | Memory Int Int deriving Show

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
  return $ Memory addr value

inputParser = M.many $ choice [parseMask,parseMem]

parseInput :: String -> [Input]
parseInput input = case M.parse inputParser "" input of
              Left _ -> []
              Right a -> a

applyMask :: String -> Int -> Int
applyMask mask value = value .&. andMask .|. orMask
  where
    andMask :: Int
    andMask = complement . foldl (\b a -> 2*b + if a == '0' then 1 else 0) 0 $ mask
    orMask = foldl (\b a -> 2*b + if a == '1' then 1 else 0) 0 mask

solvePart1 :: [Input] -> Map.Map Int Int -> String -> Int
solvePart1 [] map _ = Map.foldr (+) 0 map
solvePart1 ((Mask newMask):xs) map _ = solvePart1 xs map newMask
solvePart1 ((Memory addr value):xs) map mask = solvePart1 xs (Map.alter (\_ -> Just (applyMask mask value)) addr map) mask

part1 :: String -> Int
part1 = (\input -> solvePart1 input Map.empty "") . parseInput

part2 :: String -> Int
part2 str = 0