import qualified Data.Map.Strict as M
import Lib (Expr (..), Error (..), eval)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import Data.Either (isLeft)

testEval :: TestTree
testEval =
  testGroup "Eval" [testAdd, testSub, testMul, testDiv, testPow, testSqrt, testVar, testLet, testNested]
  where
    testEvalNoVarSuccess msg expr res =
      testCase msg $ eval M.empty expr @?= Right res
    testAdd =
      testGroup
        "Add"
        [ testCase "1 + 2 == 3" $ eval M.empty (Add (Lit 1) (Lit 2)) @?= Right 3,
          testCase "2 + 1 == 3" $ eval M.empty (Add (Lit 2) (Lit 1)) @?= Right 3,
          testCase "2 + 1 == 3 as Lit instance" $ eval M.empty (2 + 1) @?= Right 3,
          testEvalNoVarSuccess "0+2 == 2" (0 + 2) 2
        ]
    testSub =
      testGroup
        "Sub"
        [ testCase "3 - 4 == -1" $ eval M.empty (Sub (Lit 3) (Lit 4)) @?= Right (-1),
          testCase "4 - 3 == 1" $ eval M.empty (Sub (Lit 4) (Lit 3)) @?= Right 1
        ]
    testMul =
      testGroup
        "Mul"
        [ testCase "3 * 4 == 12" $ eval M.empty (Mul (Lit 3) (Lit 4)) @?= Right 12,
          testCase "4 * 3 == 12" $ eval M.empty (Mul (Lit 4) (Lit 3)) @?= Right 12
        ]
    testDiv =
      testGroup
        "Div"
        [ testCase "3 / 6 == 0.5" $ eval M.empty (Div (Lit 3) (Lit 6)) @?= Right 0.5,
          testCase "6 / 3 == 2" $ eval M.empty (Div (Lit 6) (Lit 3)) @?= Right 2,
          testCase "6 / 0 == DivisionByZero" $ eval M.empty (Div (Lit 6) (Lit 0)) @?= Left (DivisionByZero (Div (Lit 6) (Lit 0)))
        ]
    testPow =
      testGroup
        "Pow"
        [ testCase "3 ^ 4 == 81" $ eval M.empty (Pow (Lit 3) (Lit 4)) @?= Right 81,
          testCase "4 ^ 3 == 64" $ eval M.empty (Pow (Lit 4) (Lit 3)) @?= Right 64,
          testCase "-1874 ^ 0 == 1" $ eval M.empty (Pow (Lit (-1874)) (Lit 0)) @?= Right 1,
          testCase "-1 ^ 1 == -1" $ eval M.empty (Pow (Lit (-1)) (Lit 1)) @?= Right (-1),
          testCase "-1 ^ 0.5 == NegativeRoot" $ eval M.empty (Pow (Lit (-1)) (Lit 0.5)) @?= Left (NegativeRoot (Pow (Lit (-1)) (Lit 0.5)))
        ]
    testSqrt = 
      testGroup
        "Sqrt"
        [ testCase "sqrt 625 == 25" $ eval M.empty (Sqrt (Lit 625)) @?= Right 25,
          testCase "sqrt 16 == 4" $ eval M.empty (Sqrt (Lit 16)) @?= Right 4,
          testCase "sqrt -1 == NegativeRoot" $ eval M.empty (Sqrt (Lit (-1))) @?= Left (NegativeRoot (Sqrt (Lit (-1))))
        ]
    testVar =
      testGroup
        "Var"
        [ testSuccess "x + 1 == 3, x == 2" (M.singleton "x" 2) (Add (Var "x") (Lit 1)) 3,
          testFailure "x + 1 fails when x isn't assigned" M.empty (Add (Var "x") (Lit 1)),
          testSuccess "x + 1 == 3, x == 2" (M.singleton "x" 1) (Add (Var "x") (Lit 1)) 2
        ]
    testLet =
      testGroup
        "Let"
        [ testCase "let x = 2 in x + 1 == 3" $ eval M.empty (Let "x" (Lit 2) $ Add (Var "x") (Lit 1)) @?= Right 3,
          testCase "(x = 1) let x = 2 in x + 1 == 3" $ eval (M.singleton "x" 1) (Let "x" (Lit 2) $ Add (Var "x") (Lit 1)) @?= Right 3,
          testCase "let x = 1 in let x = 2 in x + 1 == 3" $ eval M.empty (Let "x" (Lit 1) $ Let "x" (Lit 2) $ Add (Var "x") (Lit 1)) @?= Right 3,
          testCase "let x = 1 + 1 in x + 1 == 3" $ eval M.empty (Let "x" (Add (Lit 1) (Lit 1)) $ Add (Var "x") (Lit 1)) @?= Right 3
        ]
    testNested =
      testGroup
        "Nested"
        [ testCase "1 + 2 + 3 == 6" $ eval M.empty (Add (Add (Lit 1) (Lit 2)) (Lit 3)) @?= Right 6,
          testCase "1 + 3 / 0 == DivisionByZero" $ eval M.empty (Add (Lit 1) (Div (Lit 3) (Lit 0))) @?= Left (DivisionByZero (Div (Lit 3) (Lit 0))),
          testCase "1 + let x = 2 in (2 + x / 4) == 3.5" $ eval M.empty (Add 1 (Let "x" (Lit 2) $ Add (Lit 2) (Div (Var "x") (Lit 4)))) @?= Right 3.5
        ]
    testSuccess msg state expr expected =
      testCase msg $
        case eval state expr of
          Right x -> assertBool "Evaluation result is wrong" (x == expected)
          Left _ -> assertFailure "Expression failed to evaluate"
    testFailure msg state expr =
      testCase msg $
        assertBool "Expr should fail to evaluate in the state" $
          isLeft $
            eval state expr

main :: IO ()
main =
  defaultMain $ testGroup "Expressions" [testEval]