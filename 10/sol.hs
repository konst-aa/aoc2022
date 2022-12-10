{-# LANGUAGE ViewPatterns #-}

import Control.Monad.State
import Data.List
import qualified Data.Set as Set
import System.IO

-- i needed to learn state at one point
type Clock = (Int, Int, Bool, [Int], [String])

-- note that the draw thing will be reversed
nextClock :: Set.Set Int -> [String] -> State Clock ([Int], [String])
nextClock s l = do
  (reg, current, on, running, draw) <- get
  let next = current + 1
  let newRunning =
        if Set.member next s
          then (next * reg) : running
          else running
  let drawPos = next `mod` 40 -- [COPE] there's a bit of garbage at the end but all good
  let adding =
        if drawPos >= reg && drawPos <= reg + 2
          then "#"
          else "."
  put (reg, next, on, newRunning, adding : draw)
  clocks s l

clocks :: Set.Set Int -> [String] -> State Clock ([Int], [String])
clocks _ [] = do
  (_, _, _, running, draw) <- get
  return (running, draw)
clocks s ((stripPrefix "noop" -> Just dir):rest) = do
  nextClock s rest
clocks s ((stripPrefix "addx " -> Just n):rest) = do
  (reg, current, on, running, draw) <- get
  if on
    then do
      put (reg + read n, current, False, running, draw)
      nextClock s rest
    else do
      put (reg, current, True, running, draw)
      nextClock s (("addx " ++ n) : rest)
clocks _ _ = do
  (_, _, _, running, draw) <- get
  return (running, draw)

-- https://stackoverflow.com/a/12876438
gr :: Int -> [a] -> [[a]]
gr _ [] = []
gr n l
  | n > 0 = take n l : gr n (drop n l)
  | otherwise = error "negative or 0 n"

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let (ns, drawn) =
        evalState
          (clocks (Set.fromList [20, 60, 100, 140, 180, 220]) $ lines contents)
          (1, 1, False, [], ["#"])
  print $ sum ns
  putStr $ concatMap ((++ "\n") . concat) $ gr 40 $ reverse drawn
