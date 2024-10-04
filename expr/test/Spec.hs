import qualified Data.Map.Strict as M
import Data.Maybe (isNothing)
import Expr (Expr (..), eval)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

testEval :: TestTree
testEval =
  testGroup "Eval" [testAdd, testVar]
  where
    testEvalNoVarSuccess msg expr res =
      testCase msg $ eval M.empty expr @?= Just res
    testAdd =
      testGroup
        "Add"
        [ testCase "1 + 2 == 3" $ eval M.empty (Plus (Lit 1) (Lit 2)) @?= Just 3,
          testCase "2 + 1 == 3" $ eval M.empty (Plus (Lit 2) (Lit 1)) @?= Just 3,
          testCase "2 + 1 == 3 as Lit instance" $ eval M.empty (2 + 1) @?= Just 3,
          testEvalNoVarSuccess "0+2 == 2" (0 + 2) 2
        ]
    testVar =
      testGroup
        "Var"
        [ testSuccess "x + 1 == 3, x == 2" (M.singleton "x" (Lit 2)) (Plus (Var "x") (Lit 1)) 3,
          testFailure "x + 1 fails when x isn't assigned" M.empty (Plus (Var "x") (Lit 1)),
          testSuccess "x + 1 == 3, x == 2" (M.singleton "x" (Lit 1)) (Plus (Var "x") (Lit 1)) 2
        ]
    testSuccess msg state expr expected =
      testCase msg $
        case eval state expr of
          Just x -> assertBool "Evaluation result is wrong" (x == expected)
          Nothing -> assertFailure "Expression failed to evaluate"
    testFailure msg state expr =
      testCase msg $
        assertBool "Expr should fail to evaluate in the state" $
          isNothing $
            eval state expr

main :: IO ()
main =
  defaultMain $ testGroup "Expressions" [testEval]