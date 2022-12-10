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

red ::
     (String -> String)
  -> Array Int String
  -> Maybe [String]
  -> Array Int String
red act acc (Just [times, start, dest]) =
  acc // [(intStart, newStartStack), (intDest, newDestStack)]
  where
    intTimes = read times
    intStart = read start
    intDest = read dest
    newStartStack = drop intTimes $ acc ! intStart
    newDestStack = act (take intTimes (acc ! intStart)) ++ (acc ! intDest)
red act acc _ = acc

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let (stackStr:actionStr:_) = splitRegex (mkRegex "\n\n") contents
  let stacks = map (parseRow . (" " ++)) $ wordsWhen (== '\n') stackStr
  let actions =
        map (matchRegex (mkRegex "move ([0-9]*) from ([0-9]*) to ([0-9]*)")) $
        wordsWhen (== '\n') actionStr
  let accArray =
        array (1, 9) $
        zip [1 .. 9] $
        map (head . wordsWhen (== ' ')) $
        transpose $ take (length stacks - 1) stacks
  let last = foldl (red reverse) accArray actions
  print $ map head $ elems last
  let last2 = foldl (red id) accArray actions
  print $ map head $ elems last2
