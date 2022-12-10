import Data.Char (digitToInt)
import Data.List
import System.IO

conv :: Bool -> Int
conv True = 1
conv False = 0

visible :: [[Int]] -> [[(Bool, Bool, Bool, Bool)]]
visible grid = zipWith4 zip4 rowsToRight rowsToLeft colsToDown colsToUp
  where
    toRight x =
      reverse $
      snd $
      foldl
        (\(prevMax, acc) curr -> (max curr prevMax, (curr > prevMax) : acc))
        (-1, [])
        x
    toLeft x =
      snd $
      foldr
        (\curr (prevMax, acc) -> (max curr prevMax, (curr > prevMax) : acc))
        (-1, [])
        x
    rot270 = transpose . transpose . transpose
    rowsToRight = map toRight grid
    rowsToLeft = map toLeft grid
    colsToUp = rot270 $ map toRight $ transpose grid
    colsToDown = rot270 $ map toLeft $ transpose grid

-- ok idt i can come up with a n^2 sol for p2 so...
toStop :: Int -> [Int] -> Int
toStop n (x:xs)
  | n <= x = 1
  | otherwise = 1 + toStop n xs
toStop _ _ = 0

taker :: [Int] -> [Int]
taker [] = []
taker (x:xs) = toStop x xs : taker xs

p2 :: [[Int]] -> [[(Int, Int, Int, Int)]]
p2 grid = zipWith4 zip4 rowsToRight rowsToLeft colsToDown colsToUp
  where
    toLeft x = reverse $ taker $ reverse x
    rot270 = transpose . transpose . transpose
    rowsToRight = map taker grid
    rowsToLeft = map toLeft grid
    colsToUp = rot270 $ map taker $ transpose grid
    colsToDown = rot270 $ map toLeft $ transpose grid

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let grid = map (map digitToInt) $ lines contents
  print $
    sum $
    map (sum . map (conv . (\(a, b, c, d) -> a || b || c || d))) $ visible grid
  print $
    maximum $ map (maximum . map (\(a, b, c, d) -> a * b * c * d)) $ p2 grid
