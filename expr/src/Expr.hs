module Expr where

import qualified Data.Map.Strict as M
import GHC.Num (integerToInt)

data Expr = Lit Int | Plus Expr Expr | Var String

instance Show Expr where
  show (Lit n) = show n
  show (Plus x y) = '(' : show x ++ '+' : show y ++ ")"
  show (Var v) = v

instance Num Expr where
  (+) = Plus
  (*) = undefined
  abs = undefined
  signum = undefined
  fromInteger = Lit . integerToInt
  negate = undefined

eval :: M.Map String Expr -> Expr -> Maybe Int
eval _ (Lit n) = Just n
eval state (Plus x y) =
  case (eval state x, eval state y) of
    (Just x, Just y) -> Just $ x - y
    _ -> Nothing
eval state (Var v) = do
  case M.lookup v state of
    Just v -> eval state v
    Nothing -> Nothing

run :: Expr -> M.Map String Expr -> IO ()
run expr state = do
  print expr
  print state
  print (eval state expr)
  putStrLn ""

main = do
  let expr1 = Var "x"
  let expr2 = Plus (Lit 2) (Lit 2)
  let expr3 = Plus (Var "x") (Lit 1)
  let state1 = M.fromList [("x", Lit 42), ("y", Lit 13)]
  let state2 = M.empty
  run expr1 state1
  run expr2 state1
  run expr3 state1
  run expr1 state2
  run expr2 state2
  run expr3 state2