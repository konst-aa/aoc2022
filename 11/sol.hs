import Data.Array
import Data.Functor
import Data.List
import Data.List.Split
import System.IO
import Text.Regex

data Monkey = Monkey Int [Int] (Int->Int) (Int->Int)

-- https://stackoverflow.com/a/4981265
parseMonkey :: String -> (Int, Monkey)
parseMonkey s =
  (divisor, Monkey 0 items operation check)
  where
    components = splitOn "\n" s
    match regStr str = matchRegex (mkRegex regStr) str <&> head
    Just items = do
      m <- match "Starting items: (.*)" $ components !! 1
      pure $ map read $ splitOn ", " m
    Just operation = do
      m <- matchRegex (mkRegex "Operation: new = (.*) (\\*|\\+|\\-|\\/) (.*)$") $ components !! 2
      let [f, op_str, s] = m
      let op = case op_str of
              "*" -> (*)
              "+" -> (+)
      pure $ \x -> (if f == "old" then x else read f) `op` (if s == "old" then x else read s)
    Just divisor = do
      m <- match "Test: divisible by (.*)$" $ components !! 3
      pure $ read m
    Just check = do
      success <- match "If true: throw to monkey (.*)$" $ components !! 4
      failure <- match "If false: throw to monkey (.*)$" $ components !! 5
      pure $ \x -> read $ if x `rem` divisor == 0 then success else failure

oneMonkey :: (Int -> Int)-> Int -> Array Int Monkey -> Array Int Monkey
oneMonkey extraOp n monkeys = foldl passer monkeys items // [(n, Monkey (inspects + length items) [] operation check)]
  where
    Monkey inspects items operation check = monkeys ! n
    passer ms item = ms // [(recipient, Monkey ins (it ++ [new_worry]) o c)] -- pretty sure updates are O(n) but doesn't matter
      where
        new_worry = extraOp $ operation item
        recipient = check new_worry
        Monkey ins it o c = ms ! recipient

round :: (Int -> Int) -> Array Int Monkey -> Array Int Monkey
round extraOp ms = foldl (\acc (i, _)-> oneMonkey extraOp i acc) ms $ assocs ms

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let parsedMonkeys = map parseMonkey $ splitOn "\n\n" contents
  let monkeys = listArray (0, length parsedMonkeys - 1) $ map snd parsedMonkeys
  let afterRounds = iterate (Main.round (`div` 3)) monkeys !! 20
  print $ product $ take 2 $ reverse $ sort $ map (\(Monkey ins _ _ _) -> ins) $ elems afterRounds 

  let lcm = product $ map fst parsedMonkeys
  let afterRounds2 = iterate (Main.round (`rem` lcm)) monkeys !! 10000
  print $ product $ take 2 $ reverse $ sort $ map (\(Monkey ins _ _ _) -> ins) $ elems afterRounds2
