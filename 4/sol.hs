import qualified Data.Set as Set
import System.IO

-- https://stackoverflow.com/a/4981265
wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =
  case dropWhile p s of
    "" -> []
    s' -> w : wordsWhen p s''
      where (w, s'') = break p s'

mapper :: [[Int]] -> Bool
mapper [[a1, a2], [b1, b2]] =
  not ((a1 == b1) && a2 == b2) && (a1 >= b1 && a2 <= b2) ||
  (a1 <= b1 && a2 >= b2)
mapper _ = False

mapper2 :: [[Int]] -> Bool
mapper2 [[a1, a2], [b1, b2]] = (a1 >= b1 && a1 <= b2) || (a1 <= b1 && a2 >= b1)
mapper2 _ = False

conv :: Bool -> Int
conv True = 1
conv False = 0

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let splits =
        map (map (map read . wordsWhen (== '-')) . wordsWhen (== ',')) $
        lines contents
  print $ sum $ map (conv . mapper) splits
  print $ sum $ map (conv . mapper2) splits
