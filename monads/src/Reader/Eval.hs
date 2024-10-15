module Reader.Eval where

import Reader.MyReader
import Expr
import qualified Data.Map as M

type VarMap a = M.Map a Int

eval :: Ord a => Expr a -> MyReader (VarMap a) (Maybe Int)
eval (Num x) = undefined
eval (Var v) = undefined
eval (Plus x y) = undefined
eval (Let x v b) = undefined

runEval :: (Ord a) => Expr a -> Maybe Int 
runEval e = runMyReader (eval e) M.empty 