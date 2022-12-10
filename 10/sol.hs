{-# LANGUAGE ViewPatterns #-}

import Control.Monad.State
import qualified Data.Array as Array
import Data.List
import qualified Data.Set as Set
import System.IO

-- i needed to learn state at one point
type Clock = (Int, [Int])

-- rewrote to be more modular
-- after seeing 1nk's sol
clocks :: [String] -> State Clock [Int]
clocks [] = do
  (current, regHist) <- get
  return regHist
clocks ((stripPrefix "noop" -> Just dir):rest) = do
  (current, regHist) <- get
  put (current + 1, head regHist : regHist)
  clocks rest
clocks ((stripPrefix "addx " -> Just n):rest) = do
  (current, regHist) <- get
  put (current + 1, (read n + head regHist) : head regHist : regHist)
  clocks rest
clocks _ = do
  (current, regHist) <- get
  return regHist

-- https://stackoverflow.com/a/12876438
gr :: Int -> [a] -> [[a]]
gr _ [] = []
gr n l
  | n > 0 = take n l : gr n (drop n l)
  | otherwise = error "negative or 0 n"

p2 :: Array.Array Int Int -> Int -> Char
p2 arr n
  | rowPos >= x - 1 && rowPos <= x + 1 = '#'
  | otherwise = '.'
  where
    x = arr Array.! n
    rowPos = ((n - 1) `mod` 40) + 0 -- rowpos is 0 indexed

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let res = evalState (clocks $ lines contents) (0, [1])
  let clockArr = Array.array (1, 240) $ zip [1 .. 240] $ reverse res
  print $ sum $ map (\n -> n * clockArr Array.! n) [20, 60, 100, 140, 180, 220]
  putStr $ unlines $ gr 40 $ map (p2 clockArr) $ Array.indices clockArr
