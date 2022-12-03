import Data.List
import Data.Map
import System.IO

aliases = fromList [("X", "A"), ("Y", "B"), ("Z", "C")]
bonuses = fromList [("A", 1), ("B", 2), ("C", 3)]
winners = fromList [("A", "C"), ("C", "B"), ("B", "A")]
losers = fromList $ foldrWithKey (\k a b -> (a, k):b) [] winners

judge :: [String] -> Int
judge [opponent, second] = do
  let you = aliases ! second
  let resultVal =
        case you of
          _
            | you == opponent -> 3
            | winners ! you == opponent -> 6
            | otherwise -> 0
  resultVal + bonuses ! you

judge any = 0

judge2 :: [String] -> Int
judge2 [opponent, res] 
  | res == "X" = bonuses ! (winners ! opponent)
  | res == "Y" = 3 + bonuses ! opponent
  | res == "Z" = 6 + bonuses ! (losers ! opponent)


judge2 any = 0

main :: IO()
main = do
  contents <- readFile "input.txt"
  let rounds = Data.List.map words $ lines contents
  print $ sum $ Data.List.map judge rounds
  print $ sum $ Data.List.map judge2 rounds
