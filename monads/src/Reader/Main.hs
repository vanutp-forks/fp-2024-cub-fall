module Reader.Main where 

import Reader.Eval (runEval)
import Expr 
import Util 
import Text.Printf 

main :: IO () 
main = do 
    putStrLn (thickEnclose "Eval examples")
    mapM_ run exprs
  where 
    run :: Expr String -> IO () 
    run e = do 
      print e 
      putStrLn $ printf "Result: %s\n" (show $ runEval e)