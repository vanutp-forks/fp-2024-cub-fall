module State.ConservativeRenamer where 

import State.MyState ( MyState(runMyState), get, gets, put, modify ) 
import Expr ( Expr(..) ) 
import qualified Data.Map as M 
import Text.Printf 

data RenameState a = RenameState { getMap :: M.Map a String, getVars :: M.Map a Int }

rename :: Expr String -> MyState (RenameState String) (Expr String)
rename (Num x) = undefined
rename (Var v) = undefined
rename (Plus x y) = undefined
rename (Let v x b) = undefined

runRename :: Expr String -> Expr String
runRename expr = 
  snd $ runMyState (rename expr) (RenameState { getMap = M.empty, getVars = M.empty })