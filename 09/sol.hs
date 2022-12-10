{-# LANGUAGE ScopedTypeVariables #-}

import Data.List
import qualified Data.Set as Set
import System.IO

type Segment = (Int, Int)

-- done with cameron, esp he explained how i misread the problem lmao
dirVec :: String -> Segment
dirVec "R" = (1, 0)
dirVec "L" = (-1, 0)
dirVec "U" = (0, 1)
dirVec "D" = (0, -1)
dirVec _ = (0, 0)

conv :: Bool -> Int
conv True = 1
conv False = 0

shift :: Segment -> Segment -> Segment
shift (tr, ty) (cr, cy) = (dup - ddown, dright - dleft)
  where
    dup = conv $ tr > cr
    ddown = conv $ tr < cr
    dright = conv $ ty > cy
    dleft = conv $ ty < cy

addTwo (a, b) (c, d) = (a + c, b + d)

applyMove :: Segment -> [Segment] -> [Segment]
applyMove curr (head:tail) =
  if apart head curr
    then curr : applyMove (addTwo head $ shift curr head) tail
    else curr : head : tail
  where
    apart (a, b) (c, d) = abs (a - c) > 1 || abs (b - d) > 1
applyMove curr _ = [curr]

takeLast :: Int -> [a] -> [a]
takeLast n l = drop (length l - n) l

dropLast :: Int -> [a] -> [a]
dropLast n l = take (length l - n) l

sol1 :: [Segment] -> Set.Set (Int, Int) -> [(String, Int)] -> Set.Set (Int, Int)
sol1 snake s ((direction, 0):xs) = sol1 snake s xs
sol1 (hs:ts) s ((direction, dist):xs) =
  sol1
    newSnake
    (Set.union s $ Set.fromList $ takeLast 1 newSnake)
    ((direction, dist - 1) : xs)
  where
    newSnake = applyMove (addTwo hs $ dirVec direction) ts
sol1 sn s _ = s

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let inp =
        map ((\[direction, dist] -> (direction, read dist :: Int)) . words) $
        lines contents
  print $ length $ sol1 [(0, 0), (0, 0)] (Set.fromList [(0, 0)]) inp
  print $ length $ sol1 (replicate 10 (0, 0)) (Set.fromList [(0, 0)]) inp
