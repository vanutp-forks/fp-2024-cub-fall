module Main where

import Control.Monad (unless)
import Text.Printf (printf)

data Expr
  = Number Double
  | Sqrt Expr
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Pow Expr Expr
  deriving (Show, Eq)

data Error
  = NegativeRoot Expr
  | DivisionByZero Expr
  deriving (Eq)

instance Show Error where
  show (NegativeRoot expr) = "Root of negative number: " ++ show expr
  show (DivisionByZero expr) = "Division by zero: " ++ show expr

eval :: Expr -> Either Error Double
eval (Number x) = Right x
eval (Sqrt sub) = case eval sub of
  Right x
    | x < 0 -> Left (NegativeRoot (Sqrt sub))
    | otherwise -> Right (sqrt x)
  Left e -> Left e
eval expr =
  let 
    evalBinOp op x y = 
      case (eval x, eval y) of
        (Right x, Right y) -> Right (op x y)
        (Left e, _) -> Left e
        (_, Left e) -> Left e
  in case expr of
    Add x y -> evalBinOp (+) x y
    Sub x y -> evalBinOp (-) x y
    Mul x y -> evalBinOp (*) x y
    Div x y -> case eval y of
      Right y | y == 0 -> Left (DivisionByZero expr)
      _ -> evalBinOp (/) x y
    Pow x y -> case (eval x, eval y) of 
      (Right x, Right y) | x < 0 && 0 < abs y && abs y < 1 -> Left (NegativeRoot expr)
      _ -> evalBinOp (**) x y


cases :: [(Expr, Either Error Double)]
cases = 
  [ (Number 1874, Right 1874)
  , (Sqrt (Number 16), Right 4)
  , (Add (Number 3) (Number 4), Right 7)
  , (Sub (Number 3) (Number 4), Right (-1))
  , (Mul (Number 3) (Number 4), Right 12)
  , (Div (Number 3) (Number 4), Right 0.75)
  , (Pow (Number 3) (Number 4), Right 81)
  , (Sqrt (Number (-1)), Left (NegativeRoot (Sqrt (Number (-1)))))
  , (Div (Number 3) (Number 0), Left (DivisionByZero (Div (Number 3) (Number 0))))
  , (Add (Number 1874) (Div (Number 3) (Number 0)), Left (DivisionByZero (Div (Number 3) (Number 0))))
  , (Pow (Number (-1)) (Number 0), Right 1)
  , (Pow (Number (-1)) (Number 1), Right (-1))
  , (Pow (Number (-1)) (Number 0.5), Left (NegativeRoot (Pow (Number (-1)) (Number 0.5))))
  ]

test :: Expr -> Either Error Double -> IO ()
test expr expected =
    let actual = eval expr in 
    unless (expected == actual) $ describeFailure actual
  where
    describeFailure actual =
      printf "eval (%s) should be %s but it was %s" (show expr) (show expected) (show actual)

main :: IO ()
main = do
  mapM_ (uncurry test) cases
  putStrLn "Done" 
