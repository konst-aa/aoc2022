import qualified Data.Set as Set
import Data.Char (ord, isLower)
import System.IO


mapper :: String -> Char
mapper st = 
  head $ Set.elems $ Set.fromList (take half st) `Set.intersection` Set.fromList (drop half st) 
    where half = length st `div` 2

mapper2 :: [String] -> Char
mapper2 items = 
  head $ Set.elems $ foldl (Set.intersection) (head itemSets) itemSets
    where itemSets = map Set.fromList $ items

moreOrd :: Char -> Int
moreOrd chr 
  | isLower chr = o - 96
  | otherwise = o + 32 + 26 - 96
  where o = ord chr

-- https://stackoverflow.com/a/12876438
group :: Int -> [a] -> [[a]]
group _ [] = []
group n l
  | n > 0 = (take n l) : (group n (drop n l))
  | otherwise = error "negative or 0 n"

main :: IO()
main = do
  contents <- readFile "input.txt"
  print $ sum $ map (moreOrd . mapper) $ lines contents
  print $ sum $ map (moreOrd . mapper2) $ group 3 $ lines contents
