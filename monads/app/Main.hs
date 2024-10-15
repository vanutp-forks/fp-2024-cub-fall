module Main (main) where

import Expr
import qualified Reader.Main as Reader 
import qualified Writer.Main as Writer 
import qualified State.Main as State 

main :: IO ()
main = do 
  Reader.main 
  State.main 
  Writer.main
