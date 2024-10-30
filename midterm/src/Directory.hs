module Directory where

import qualified Data.Set as Set
import System.Directory (doesDirectoryExist, listDirectory)

data DirectoryTree 
  = Node FilePath (Set.Set DirectoryTree)
  | Leaf FilePath
  deriving (Show, Eq)

instance Ord DirectoryTree where
    compare (Leaf a) (Leaf b) = compare a b
    compare (Leaf a) (Node b _) = compare a b
    compare (Node a _) (Leaf b) = compare a b
    compare (Node a _) (Node b _) = compare a b

-- Create a tree that represents the hierarchy of files and subdirectories of the given directory.
buildTree :: FilePath -> IO DirectoryTree
buildTree path = do
    exists <- doesDirectoryExist path
    if exists then do
        contents <- listDirectory path
        subtrees <- mapM buildTree contents
        return $ Node path (Set.fromList subtrees)
    else return $ Leaf path

-- Implement a function that displays the subdirectories of the current directory before the files in it. 
defaultDisplayTree :: DirectoryTree -> String
defaultDisplayTree = undefined 

-- Implement a function that displays the subdirectories and the files in it in the alphabet order. 
alphabetisedDisplayTree :: DirectoryTree -> String
alphabetisedDisplayTree = undefined 