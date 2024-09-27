module Tree where

import Data.Either (fromRight)
import Data.Maybe (catMaybes, isNothing)
import Text.Read (readMaybe)

data Tree a
  = E
  | N (Tree a) a (Tree a)
  deriving (Show, Eq, Read)

insert :: (Ord a) => a -> Tree a -> Tree a
insert x E = N E x E
insert x (N l a r)
  | x == a = N l a r
  | x <= a = N (insert x l) a r
  | otherwise = N l a (insert x r)

data ParseResult
  = IntList [Int]
  | IntTree (Tree Int)

parse :: String -> Either String ParseResult
parse input =
  case readMaybe @(Tree Int) input of
    Just t -> Right $ IntTree t
    Nothing ->
      case parseIntList (words input) of
        Just xs -> Right $ IntList xs
        _ -> Left "Please input either a list of ints of a tree"
  where
    parseIntList [] = Just []
    parseIntList (x : xs) =
      case readMaybe @Int x of
        Just i ->
          case parseIntList xs of
            Just xs -> Just (i : xs)
            _ -> Nothing
        _ -> Nothing
