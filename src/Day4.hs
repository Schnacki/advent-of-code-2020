module Day4 (solvePart1, solvePart2, parseInput, part1, part2) where

import Data.Bifunctor (second)
import Data.Char (isDigit, isHexDigit)
import Data.List ((\\))
import Data.List.Split (splitWhen)
import Data.Maybe(catMaybes)
import Text.Read (readMaybe)

data Passport = Passport { byr :: Int, iyr :: Int, eyr :: Int, hgt :: String, hcl :: String, ecl :: String, pid :: String } deriving Show

parsePassport :: [(String, String)] -> Maybe Passport
parsePassport pwd = do
  byr <- lookup "byr" pwd >>= readMaybe
  iyr <- lookup "iyr" pwd >>= readMaybe
  eyr <- lookup "eyr" pwd >>= readMaybe
  hgt <- lookup "hgt" pwd
  hcl <- lookup "hcl" pwd
  ecl <- lookup "ecl" pwd
  pid <- lookup "pid" pwd
  return $ Passport byr iyr eyr hgt hcl ecl pid

parseInput :: String -> [Maybe Passport]
parseInput = map (parsePassport . toTuples . words . unwords) . splitWhen (== "") . lines
  where
    toTuples = map (second tail . span (/= ':'))

solvePart1 :: String -> Int
solvePart1 = length . catMaybes . parseInput

part1 :: FilePath -> IO ()
part1 file = readFile file >>= print . solvePart1


passportIdValid :: Passport -> Bool
passportIdValid pwd = length (pid pwd) == 9 && all isDigit (pid pwd)

eyeColorValid :: Passport -> Bool
eyeColorValid pwd = (ecl pwd) `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

hairColorValid :: Passport -> Bool
hairColorValid pwd = let (hash : number) = hcl pwd in
  hash == '#' && length number == 6 && all isHexDigit number

birthYearValid :: Passport -> Bool
birthYearValid pwd = (byr pwd) >= 1920 && (byr pwd) <= 2002

issuerYearValid :: Passport -> Bool
issuerYearValid pwd = (iyr pwd) >= 2010 && (iyr pwd) <= 2020

expirationYearValid :: Passport -> Bool
expirationYearValid pwd = (eyr pwd) >= 2020 && (eyr pwd) <= 2030

heightValid :: Passport -> Bool
heightValid pwd = let (height, unit) = span isDigit (hgt pwd)
   in case unit of
        "cm" -> (read height :: Int) >= 150 && (read height :: Int) <= 193
        "in" -> (read height :: Int) >= 59 && (read height :: Int) <= 76
        _ -> False

passportDataValid2 :: Passport -> Bool
passportDataValid2 pwd = and $ [passportIdValid, eyeColorValid, hairColorValid, birthYearValid, issuerYearValid, expirationYearValid, heightValid] <*> [pwd]

solvePart2 :: [Passport] -> Int
solvePart2 = length . filter passportDataValid2

part2 :: FilePath -> IO ()
part2 file = readFile file >>= print . solvePart2 . catMaybes . parseInput