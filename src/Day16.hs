module Day16 (part1, part2, Rule (..), solvePart1, parseInput) where

import Control.Applicative ((<|>))
import Data.Ix (inRange)
import Data.List (transpose)
import Data.Void (Void)
import Text.Megaparsec (Parsec, parse, sepBy, some)
import Text.Megaparsec.Char (alphaNumChar, char, space, string)
import Text.Megaparsec.Char.Lexer (decimal)

type Parser = Parsec Void String

parseRule :: Parser Rule
parseRule = do
  name <- some (alphaNumChar <|> char ' ')
  char ':'
  space
  a <- decimal
  char '-'
  b <- decimal
  string " or "
  c <- decimal
  char '-'
  d <- decimal
  char '\n'
  return $ Rule name [(a, b), (c, d)]

parseList = do
  ticket <- decimal `sepBy` char ','
  char '\n'
  return ticket

parseTicket :: Parser ([Rule], [Int], [[Int]])
parseTicket = do
  rules <- some parseRule
  string "\nyour ticket:\n"
  ticket <- parseList
  string "\nnearby tickets:\n"
  nearybyTickets <- some parseList
  return (rules, ticket, nearybyTickets)

parseInput input = case parse parseTicket "" input of
  Left _ -> error "LOL"
  Right a -> a

data Rule = Rule {fieldName :: String, ranges :: [(Int, Int)]} deriving (Show)

ruleApplies :: Int -> Rule -> Bool
ruleApplies i = any (`inRange` i) . ranges

fieldInvalid :: Int -> [Rule] -> Bool
fieldInvalid field = not . any (ruleApplies field)

solvePart1 :: [Int] -> [Rule] -> Int
solvePart1 fields rules = sum $ filter (`fieldInvalid` rules) fields

part1 :: String -> Int
part1 input =
  let (rules, _, tickets) = parseInput input
   in solvePart1 (concat tickets) rules

part2 :: String -> Int
part2 input = 0