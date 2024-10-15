module State.Renamer where 

import State.MyState ( MyState(runMyState), get, put, gets ) 
import Expr ( Expr(..) )
import qualified Data.Map as M 

data RenameState a = RenameState { getMap :: M.Map a Int, getVar :: Int }

rename :: (Ord a, Show a) => Expr a -> MyState (RenameState a) (Expr Int)
rename (Num x) = undefined
rename (Var v) = undefined
rename (Plus x y) = undefined
rename (Let v x b) = undefined

runRename :: (Ord a, Show a) => Expr a -> Expr Int
runRename expr = 
  snd $ runMyState (rename expr) (RenameState { getMap = M.empty, getVar = 0 })
