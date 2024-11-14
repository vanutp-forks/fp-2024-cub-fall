import Test.Tasty

import qualified Test.Compiler
import qualified Test.Interpreter

main :: IO ()
main = do
  defaultMain (testGroup "All Tests"
                [ testGroup "Compiler" Test.Compiler.props
                , testGroup "Interpreter" Test.Interpreter.props
                ])