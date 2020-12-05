module Day4 (solvePart1, solvePart2, parseInput, part1, part2) where

import Data.Bifunctor (second)
import Data.Char (isDigit, isHexDigit)
import Data.List ((\\))
import Data.List.Split (splitWhen)
import Data.Maybe(catMaybes)
import Text.Read (readMaybe)

data Passport = Passport { byr :: Int, iyr :: Int, eyr :: Int, hgt :: String, hcl :: String, ecl :: String, pid :: String } deriving Show

parsePassport :: [(String, String)] -> Maybe Passport
parsePassport p = do
  byr <- lookup "byr" p >>= readMaybe
  iyr <- lookup "iyr" p >>= readMaybe
  eyr <- lookup "eyr" p >>= readMaybe
  hgt <- lookup "hgt" p
  hcl <- lookup "hcl" p
  ecl <- lookup "ecl" p
  pid <- lookup "pid" p
  return $ Passport byr iyr eyr hgt hcl ecl pid

parseInput :: String -> [Maybe Passport]
parseInput = map (parsePassport . map (second tail . span (/= ':')) . words . unwords) . splitWhen (== "") . lines

solvePart1 :: String -> Int
solvePart1 = length . catMaybes . parseInput

part1 :: FilePath -> IO ()
part1 file = readFile file >>= print . solvePart1


passportIdValid :: Passport -> Bool
passportIdValid p = length (pid p) == 9 && all isDigit (pid p)

eyeColorValid :: Passport -> Bool
eyeColorValid p = (ecl p) `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

hairColorValid :: Passport -> Bool
hairColorValid p = let (hash : number) = hcl p in
  hash == '#' && length number == 6 && all isHexDigit number

birthYearValid :: Passport -> Bool
birthYearValid p = (byr p) >= 1920 && (byr p) <= 2002

issuerYearValid :: Passport -> Bool
issuerYearValid p = (iyr p) >= 2010 && (iyr p) <= 2020

expirationYearValid :: Passport -> Bool
expirationYearValid p = (eyr p) >= 2020 && (eyr p) <= 2030

heightValid :: Passport -> Bool
heightValid p = let (height, unit) = span isDigit (hgt p)
   in case unit of
        "cm" -> (read height :: Int) >= 150 && (read height :: Int) <= 193
        "in" -> (read height :: Int) >= 59 && (read height :: Int) <= 76
        _ -> False

passportDataValid :: Passport -> Bool
passportDataValid p = and $ [passportIdValid, eyeColorValid, hairColorValid, birthYearValid, issuerYearValid, expirationYearValid, heightValid] <*> [p]

solvePart2 :: [Passport] -> Int
solvePart2 = length . filter passportDataValid

part2 :: FilePath -> IO ()
part2 file = readFile file >>= print . solvePart2 . catMaybes . parseInput