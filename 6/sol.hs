import System.IO
import qualified Data.Set as Set

red :: Int-> Int -> String -> Int
red n curr [] = 0
red n curr els
  | length (Set.fromList $ take n els) == n = curr
  | otherwise = red n (curr + 1) (drop 1 els)

-- this would be a bit harder if the constraints were diff
main :: IO()
main = do
  contents <- readFile "input.txt"
  print $ red 4 4 contents
  print $ red 14 14 contents
