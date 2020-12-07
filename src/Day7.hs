module Day7 (part1, part2, parentBags, allParentBags) where

import Data.List(nub)
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

inputParser :: Parser [(String, [(Int, String)])]
inputParser = M.many $ do
  bag <- parseBag
  string " contain "
  bags <- try (string "no other bags" *> return []) <|> parseBagWithCount `sepBy` (string ", ")
  char '.'
  _ <- optional $ char '\n'
  return $  (bag, bags)

parseInput = M.parse inputParser ""

-----------------------

parentBags :: String -> [(String, [(Int, String)])] -> [String]
parentBags bag = fmap fst . filter ( \(_, parents) -> any (\(_, b) -> b == bag) parents)

allParentBags :: [(String, [(Int, String)])] -> String -> [String]
allParentBags allBags bag = let parents = parentBags bag allBags
  in parents ++ concatMap (allParentBags allBags) parents

part1 :: FilePath -> IO ()
part1 file = print . fmap (\bags -> length $ nub $ allParentBags bags "shiny gold") . parseInput =<< readFile file


-- TODO
part2 :: FilePath -> IO ()
part2 file = return ()