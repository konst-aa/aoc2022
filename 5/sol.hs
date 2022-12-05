import Data.Array
import Data.List (transpose)
import System.IO
import Text.Regex (matchRegex, mkRegex, splitRegex)

-- https://stackoverflow.com/a/4981265
wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =
  case dropWhile p s of
    "" -> []
    s' -> w : wordsWhen p s''
      where (w, s'') = break p s'

conv :: Bool -> Int
conv True = 1
conv False = 0

parseRow :: String -> [Char]
parseRow (sp:a:elem:b:xs) = elem : parseRow xs
parseRow _ = []

red :: Array Int String -> Maybe [String] -> Array Int String
red acc (Just [times, start, dest]) =
  acc // [(intStart, newStartStack), (intDest, newDestStack)]
  where
    intTimes = read times
    intStart = read start
    intDest = read dest
    newStartStack = drop intTimes $ acc ! intStart
    newDestStack = reverse (take intTimes (acc ! intStart)) ++ (acc ! intDest)
red acc _ = acc

red2 :: Array Int String -> Maybe [String] -> Array Int String
red2 acc (Just [times, start, dest]) =
  acc // [(intStart, newStartStack), (intDest, newDestStack)]
  where
    intTimes = read times
    intStart = read start
    intDest = read dest
    newStartStack = drop intTimes $ acc ! intStart
    newDestStack = take intTimes (acc ! intStart) ++ (acc ! intDest)
red2 acc _ = acc

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let members = splitRegex (mkRegex "\n\n") contents
  let stacks = map (parseRow . (" " ++)) $ wordsWhen (== '\n') $ head members
  let actions =
        map (matchRegex (mkRegex "move ([0-9]*) from ([0-9]*) to ([0-9]*)")) $
        wordsWhen (== '\n') $ head $ tail members
  let accArray =
        array (1, 9) $
        zip [1 .. 9] $
        map (head . wordsWhen (== ' ')) $
        transpose $ take (length stacks - 1) stacks
  let last = foldl red accArray actions
  print $ map head $ elems last
  let last2 = foldl red2 accArray actions
  print $ map head $ elems last2
