module Day22 (solvePart1, part1, part2) where

type CardDeck = [Int]

type GameState = (CardDeck, CardDeck)

findWinner :: GameState -> CardDeck
findWinner (c1 : c1s, c2 : c2s) = findWinner $ if c1 > c2 then (c1s ++ [c1, c2], c2s) else (c1s, c2s ++ [c2, c1])
findWinner ([], c) = c
findWinner (c, []) = c

solvePart1 :: GameState -> Int
solvePart1 st =
  let deck = findWinner st
   in sum $ zipWith (*) [length deck, length deck -1 ..] deck

parseInput :: String -> GameState
parseInput = cl . span (/= "") . lines
  where
    cl (_ : as, _ : _ : bs) = (map read as, map read bs)

part1 :: String -> Int
part1 = solvePart1 . parseInput

part2 :: String -> Int
part2 = solvePart1 . parseInput