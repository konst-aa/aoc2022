import Data.List
import System.IO
-- https://stackoverflow.com/questions/7867723/haskell-file-reading

red :: ([Int], [Int]) -> String -> ([Int], [Int])
red (current, all) item =
        if item == "" 
         then ([], (sum current):all)
        else ((read item):current, all)

main = do  
        contents <- readFile "input.txt"
        let (_, big) = foldl red ([], [0]) $ lines contents ++ [""]
        print $ maximum big
        print . sum $ drop (length big - 3) (sort big)
