module Day4Spec (spec) where

import Day4
import Test.Hspec

spec = return () --do
--  describe "Part 1:" $ do
--    it "\"ecl:gry pid:860033327 eyr:2020 hcl:#fffffd byr:1937 iyr:2017 cid:147 hgt:183cm\" is a valid passport." $ do
--      solvePart1 (parseInput "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd byr:1937 iyr:2017 cid:147 hgt:183cm") `shouldBe` 1
--    it "\"iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884 hcl:#cfa07d byr:1929\" is not a valid passport." $ do
--      solvePart1 (parseInput "iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884 hcl:#cfa07d byr:1929") `shouldBe` 0
--    it "\"hcl:#ae17e1 iyr:2013 eyr:2024 ecl:brn pid:760753108 byr:1931 hgt:179cm\" is a valid passport." $ do
--      solvePart1 (parseInput "hcl:#ae17e1 iyr:2013 eyr:2024 ecl:brn pid:760753108 byr:1931 hgt:179cm") `shouldBe` 1
--    it "\"hcl:#cfa07d eyr:2025 pid:166559648 iyr:2011 ecl:brn hgt:59in\" is not a valid passport." $ do
--      solvePart1 (parseInput "hcl:#cfa07d eyr:2025 pid:166559648 iyr:2011 ecl:brn hgt:59in") `shouldBe` 0
--
--  describe "Part 2:" $ do
--    it "\"eyr:1972 cid:100 hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926\" is not a valid passport." $ do
--      solvePart2 (parseInput "eyr:1972 cid:100 hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926") `shouldBe` 0
--    it "\"iyr:2019 hcl:#602927 eyr:1967 hgt:170cm ecl:grn pid:012533040 byr:1946\" is not a valid passport." $ do
--      solvePart2 (parseInput "iyr:2019 hcl:#602927 eyr:1967 hgt:170cm ecl:grn pid:012533040 byr:1946") `shouldBe` 0
--    it "\"hcl:dab227 iyr:2012 ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277\" is not a valid passport." $ do
--      solvePart2 (parseInput "hcl:dab227 iyr:2012 ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277") `shouldBe` 0
--    it "\"hgt:59cm ecl:zzz eyr:2038 hcl:74454a iyr:2023 pid:3556412378 byr:2007\" is not a valid passport." $ do
--      solvePart2 (parseInput "hgt:59cm ecl:zzz eyr:2038 hcl:74454a iyr:2023 pid:3556412378 byr:2007") `shouldBe` 0
--    it "\"pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980 hcl:#623a2f\" is a valid passport." $ do
--      solvePart2 (parseInput "pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980 hcl:#623a2f") `shouldBe` 1
--    it "\"eyr:2029 ecl:blu cid:129 byr:1989 iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm\" is a valid passport." $ do
--      solvePart2 (parseInput "eyr:2029 ecl:blu cid:129 byr:1989 iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm") `shouldBe` 1
--    it "\"hcl:#888785 hgt:164cm byr:2001 iyr:2015 cid:88 pid:545766238 ecl:hzl eyr:2022\" is a valid passport." $ do
--      solvePart2 (parseInput "hcl:#888785 hgt:164cm byr:2001 iyr:2015 cid:88 pid:545766238 ecl:hzl eyr:2022") `shouldBe` 1
--    it "\"iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719\" is a valid passport." $ do
--      solvePart2 (parseInput "iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719") `shouldBe` 1