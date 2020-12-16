module Day16 (part1, part2, parseRule,Rule(..), parseInput, solvePart1) where

import Text.Megaparsec (choice, many, optional, Parsec, parse, some, sepBy)
import Text.Megaparsec.Char (alphaNumChar, char, string, space)
import Text.Megaparsec.Char.Lexer (decimal)
import Data.Void(Void)
import Data.Ix(inRange)
import Control.Applicative( (<|>) )

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
  rules <- many parseRule
  string "\nyour ticket:\n"
  ticket <- parseList
  string "\nnearby tickets:\n"
  nearybyTickets <- some parseList
  return (rules, ticket, nearybyTickets)

parseInput input = case parse parseTicket "" input of
  Left _ -> error "LOL"
  Right a -> a

data Rule = Rule { fieldName :: String, ranges :: [(Int, Int)] } deriving Show

fieldInvalid :: Int -> [Rule] -> Bool
fieldInvalid field = not . any (ruleApplies field)
  where
    ruleApplies i = any (`inRange` i) . ranges

solvePart1 :: [Int] -> [Rule] -> Int
solvePart1 fields rules = sum $ filter (`fieldInvalid` rules) fields

part1 :: String -> Int
part1 input = let (rules, _, tickets) = parseInput input
  in solvePart1 (concat tickets) rules

part2 :: String -> Int
part2 _ = 0