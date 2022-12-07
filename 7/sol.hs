{-# LANGUAGE ViewPatterns, ScopedTypeVariables #-}
import qualified Data.Map as Map
import System.IO
import Data.List
import Data.Either

-- newtype Vals a b = (Node a b) | b
--newtype Leaf n = Leaf n
data Node a b = Branches a | Leaf b deriving (Show)
data FileTree = FileTree (Node (Map.Map String FileTree) Int) deriving (Show)

treeInsert :: [String] -> Int -> FileTree -> FileTree
treeInsert paths size (FileTree (Leaf n)) = FileTree $ Leaf n
treeInsert [new] size (FileTree (Branches tree)) =
  FileTree $ Branches $ Map.insert new (FileTree $ Leaf size) tree
treeInsert (dir:rest) size (FileTree (Branches tree)) =
  FileTree $ Branches $ Map.insert dir (treeInsert rest size $ tree Map.! dir) tree
treeInsert _ _ a = a

dispatch :: [String] -> [String] -> FileTree -> FileTree
dispatch [] _ tree = tree
dispatch ((stripPrefix "$ cd " -> Just dir):acts) path tree = 
  if dir == ".."
       then dispatch acts (drop 1 path) tree
    else dispatch acts (dir:path) tree

dispatch ("$ ls":acts) path tree = dispatch acts path tree
dispatch ((stripPrefix "dir " -> Just dir):acts) path tree = 
  dispatch acts path tree
dispatch (f:acts) path tree = dispatch acts path $ treeInsert path size tree
  where size = read $ head $ words f

main :: IO()
main = do
  contents <- readFile "t.txt"
  print $ dispatch (lines contents) [] $ FileTree $ Branches Map.empty
