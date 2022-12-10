{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

import Data.Either
import Data.List
import qualified Data.Map as Map
import System.IO

-- ill turn this into a stub one day
-- these types wouldn't exist without the help of the fp discord :pray:
data Node a b
  = Branches a
  | Leaf b
  deriving (Show)

newtype FileTree =
  FileTree (Node (Map.Map String FileTree) Int)
  deriving (Show)

{-
treeInsert :: [String] -> FileTree -> FileTree -> FileTree
treeInsert paths elem (FileTree (Leaf n)) = FileTree $ Leaf n
treeInsert [loc] elem (FileTree (Branches tree)) =
  FileTree $ Branches $ Map.insert loc elem tree
treeInsert (dir:rest) elem (FileTree (Branches tree)) =
  FileTree $
  Branches $ Map.insert dir (treeInsert rest elem $ tree Map.! dir) tree
-}
-- pretty sure this is On^2 but we get away with it
-- idk monads so I can't look up elements
treeInsert :: [String] -> FileTree -> FileTree -> FileTree
treeInsert paths elem (FileTree (Leaf n)) = FileTree $ Leaf n
treeInsert [loc] elem (FileTree (Branches tree)) =
  FileTree $ Branches $ Map.union tree $ Map.fromList [(loc, elem)]
treeInsert (dir:rest) elem (FileTree (Branches tree)) =
  FileTree $
  Branches $ Map.insert dir (treeInsert rest elem $ tree Map.! dir) tree
treeInsert _ _ a = a

dispatch :: [String] -> [String] -> FileTree -> FileTree
dispatch [] _ tree = tree
dispatch ((stripPrefix "$ cd " -> Just dir):acts) path tree =
  dispatch acts newPath $
  treeInsert (reverse newPath) (FileTree $ Branches Map.empty) tree
  where
    newPath
      | dir == ".." = drop 1 path
      | otherwise = dir : path
dispatch ("$ ls":acts) path tree = dispatch acts path tree
dispatch ((stripPrefix "dir " -> Just dir):acts) path tree =
  dispatch acts path $
  treeInsert (reverse $ dir : path) (FileTree $ Branches Map.empty) tree
dispatch (f:acts) path tree =
  dispatch acts path $
  treeInsert (reverse $ name : path) (FileTree $ Leaf size) tree
  where
    size :: Int = read $ head $ words f
    name :: String = head . tail $ words f

flattener :: FileTree -> (Int, [Int])
flattener (FileTree (Leaf size)) = (size, [])
flattener (FileTree (Branches dirs)) = (files, files : joined)
  where
    nextCall = map flattener $ Map.elems dirs
    files = sum $ map fst nextCall
    joined = concatMap snd nextCall

-- too tired
p2 :: Int -> Int -> [Int] -> Int
p2 n item (start:rest)
  | item == start = n
  | otherwise = p2 (n + 1) item rest
p2 _ _ _ = -1

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let tree = dispatch (lines contents) [] $ FileTree $ Branches Map.empty
  let (x, flatTree) = flattener tree
  print $ sum [n | n <- flatTree, n < 100000]
  let freeSpace = 70000000 - head flatTree
  let toFree = 30000000 - freeSpace
  let smalls = [n | n <- flatTree, n > toFree]
  print $ minimum smalls
