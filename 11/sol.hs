import Data.List
import Data.List.Split
import System.IO
import Text.Regex

data Monkey = Monkey [Int] (Int->Int) (Int->Bool)

-- https://stackoverflow.com/a/4981265
parseMonkey :: String -> (Int, Monkey)
parseMonkey s =
  (n, Monkey items operation $ const True)
  where 
    components = splitOn "\n" s
    Just n = do 
      m <- matchRegex (mkRegex "Monkey ([0-9]*):") $ head components
      pure $ read $ head m
    Just items = do
      m <- matchRegex (mkRegex "Starting items: (.*)") $ components !! 1
      pure $ map read $ splitOn ", " $ head m
    Just operation = do
      m <- matchRegex (mkRegex "Operation: new = (.*) (\\*|\\+|\\-|\\/) (.*)$") $ components !! 2
      let [f, op_str, s] = m
      let op = case op_str of
              "*" -> (*)
              "+" -> (+)
              "-" -> (-)
              "/" -> div
      pure $ \x -> (if f == "old" then x else read f) `op` (if s == "old" then x else read s)



main :: IO ()
main = do
  contents <- readFile "t.txt"
  let monkeys = map parseMonkey $ splitOn "\n\n" contents
  let (_, Monkey a b c) = head monkeys
  print $ b 5
