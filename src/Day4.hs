module Day4(solvePart1, solvePart2, parseInput, part1, part2) where

import Data.List((\\))
import Data.List.Split(splitWhen)
import Data.Char(isDigit, isHexDigit)
import Text.Read(readMaybe)
import Data.Bifunctor(second)

type PassportData = [(String, String)]

parseInput :: String -> [PassportData]
parseInput = map (toTuples . words . unwords) . splitWhen (== "") . lines
  where toTuples = map (second tail . span (/= ':'))

passportDataValid1 :: PassportData -> Bool
passportDataValid1 = null . (["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"] \\) . map fst

solvePart1 :: [PassportData] -> Int
solvePart1 = length . filter passportDataValid1

part1 :: FilePath -> IO ()
part1 file = readFile file >>= print . solvePart1 . parseInput

passportIdValid :: PassportData -> Bool
passportIdValid pwd = case lookup "pid" pwd of
  Nothing -> False
  Just pid -> length pid == 9 && all isDigit pid

eyeColorValid :: PassportData -> Bool
eyeColorValid pwd = case lookup "ecl" pwd of
  Nothing -> False
  Just ecl -> ecl `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

hairColorValid :: PassportData -> Bool
hairColorValid pwd = case lookup "hcl" pwd of
  Nothing -> False
  Just hcl -> hash == '#' && length number == 6 && all isHexDigit number
    where (hash:number) = hcl

birthYearValid :: PassportData -> Bool
birthYearValid pwd = case lookup "byr" pwd >>= readMaybe of
  Nothing -> False
  Just year -> year >= 1920 && year <=2002

issuerYearValid :: PassportData -> Bool
issuerYearValid pwd = case lookup "iyr" pwd >>= readMaybe of
  Nothing -> False
  Just year -> year >= 2010 && year <=2020

expirationYearValid :: PassportData -> Bool
expirationYearValid pwd = case lookup "eyr" pwd >>= readMaybe of
  Nothing -> False
  Just year -> year >= 2020 && year <=2030

heightValid :: PassportData -> Bool
heightValid pwd = case lookup "hgt" pwd of
  Nothing -> False
  Just hgt -> let (height, unit) = span isDigit hgt in
    case unit of
      "cm" -> (read height :: Int) >= 150 && (read height :: Int) <= 193
      "in" -> (read height :: Int) >= 59 && (read height :: Int) <= 76
      _ -> False


passportDataValid2 :: PassportData -> Bool
passportDataValid2 pwd = and $ [passportDataValid1, passportIdValid, eyeColorValid, hairColorValid, birthYearValid, issuerYearValid, expirationYearValid, heightValid] <*> [pwd]

solvePart2 :: [PassportData] -> Int
solvePart2 = length . filter passportDataValid2

part2 :: FilePath -> IO ()
part2 file = readFile file >>= print . solvePart2 . parseInput