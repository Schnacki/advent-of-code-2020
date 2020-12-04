module Day4(solvePart1, part1, parseInput) where

import Data.List((\\))
import Data.List.Split(splitWhen)

type PassportData = [(String, String)]

expectedKeys = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

parseInput :: String -> [PassportData]
parseInput = map toTuples . map words . map unwords . splitWhen(=="") . lines
  where toTuples = map ((\(a,b) -> (a, tail b)) . span (/=':'))

passportDataValid1 :: PassportData -> Bool
passportDataValid1 pw = (length (expectedKeys \\ (map fst pw))) == 0

solvePart1 :: [PassportData] -> Int
solvePart1 = length . filter passportDataValid1

part1 :: FilePath -> IO ()
part1 file = readFile file >>= print . solvePart1 . parseInput