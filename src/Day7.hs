module Day7 (part1, part2, solvePart1, solvePart2) where

import Data.Functor (($>))
import Data.List (lookup, nub)
import Data.Void
import Text.Megaparsec as M
import Text.Megaparsec.Char as C
import Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

parseBag :: Parser String
parseBag = do
  a <- M.some alphaNumChar
  _ <- C.space
  b <- M.some alphaNumChar
  _ <- string " bag"
  _ <- optional $ char 's'
  return $ a ++ " " ++ b

parseBagWithCount :: Parser (Int, String)
parseBagWithCount = do
  i <- L.decimal
  _ <- C.space
  bag <- parseBag
  return (i, bag)

type BagList = [(String, [(Int, String)])]

inputParser :: Parser BagList
inputParser = M.many $ do
  bag <- parseBag
  string " contain "
  bags <- try (string "no other bags" $> []) <|> parseBagWithCount `sepBy` string ", "
  char '.'
  _ <- optional $ char '\n'
  return (bag, bags)

parseInput :: String -> BagList
parseInput input = case M.parse inputParser "" input of
              Left _ -> []
              Right a -> a


parentBags :: String -> BagList -> [String]
parentBags bag = fmap fst . filter (\(_, parents) -> any (\(_, b) -> b == bag) parents)

allParentBags :: BagList -> String -> [String]
allParentBags allBags bag =
  let parents = parentBags bag allBags
   in parents ++ concatMap (allParentBags allBags) parents

solvePart1 :: BagList -> Int
solvePart1 bags = length . nub $ allParentBags bags "shiny gold"

part1 :: String -> Int
part1 = solvePart1 . parseInput

countBags :: String -> BagList -> Int
countBags bag bags = maybe 0 (foldr (+) 1 . fmap (\(i, s) -> i * countBags s bags)) . lookup bag $ bags

solvePart2 :: BagList -> Int
solvePart2 = (\n -> n - 1) . countBags "shiny gold"

part2 :: String -> Int
part2 = solvePart2 . parseInput