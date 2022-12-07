import Data.Char (isLower, ord)
import qualified Data.Set as Set
import System.IO

mapper :: String -> Set.Set Char
mapper st =
  Set.fromList (take half st) `Set.intersection` Set.fromList (drop half st)
  where
    half = length st `div` 2

mapper2 :: [String] -> Set.Set Char
mapper2 items = foldl Set.intersection (head itemSets) itemSets
  where
    itemSets = map Set.fromList items

-- cleaner after seeing cam's impl in php
moreOrd :: Char -> Int
moreOrd chr
  | isLower chr = o - ord 'a'
  | otherwise = o - ord 'A' + 26
  where
    o = ord chr + 1

-- https://stackoverflow.com/a/12876438
group :: Int -> [a] -> [[a]]
group _ [] = []
group n l
  | n > 0 = take n l : group n (drop n l)
  | otherwise = error "negative or 0 n"

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let prep = moreOrd . head . Set.elems
  print $ sum $ map (prep . mapper) $ lines contents
  print $ sum $ map (prep . mapper2) $ group 3 $ lines contents
