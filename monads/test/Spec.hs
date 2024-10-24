import qualified Data.Map.Strict as M
import Expr (Expr(..))
import HW.Compiler (compile)
import HW.StackMachine (StackInstr(..), StackProgram)
import HW.Eval (MachineState(..), Error(..), initialState, execProgram, execInstr)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import State.MyState (MyState(runMyState))

testsCompile :: TestTree
testsCompile =
  testGroup "Compile" [testNum, testVar, testPlus, testLet, testComplex]
  where
    testNum = testCompile "Num" (Num 1) [PushNum 1]
    testVar = testCompile "Var x" (Var "x") [PushVar "x"]
    testPlus =
      testGroup
        "Plus"
        [ testCompile "3 + 4" (Plus (Num 3) (Num 4)) [PushNum 3, PushNum 4, Add],
          testCompile "3 + x" (Plus (Num 3) (Var "x")) [PushNum 3, PushVar "x", Add]
        ]
    testLet =
      testGroup
        "Let"
        [ testCompile "let x = 3 in x"
            (Let "x" (Num 3) (Var "x"))
            [PushNum 3, StoreVar "x", PushVar "x"],
          testCompile "let x = 3 in x + 4" 
            (Let "x" (Num 3) (Plus (Var "x") (Num 4)))
            [PushNum 3, StoreVar "x", PushVar "x", PushNum 4, Add]
        ]
    testComplex =
      testGroup
        "Complex"
        [ testCompile 
            "let x = 13 in let y = 42 in let x = (x + y) in (x + y)"
            (Let "x" (Num 13) $ Let "y" (Num 42) $ Let "x" (Plus (Var "x") (Var "y")) $ Plus (Var "x") (Var "y"))
            [PushNum 13,StoreVar "x",PushNum 42,StoreVar "y",PushVar "x",PushVar "y",Add,StoreVar "x",PushVar "x",PushVar "y",Add],
          testCompile
            "let x = 13 in let x = 42 in let x = (x + x) in (x + x)"
            (Let "x" (Num 13) $ Let "x" (Num 42) $ Let "x" (Plus (Var "x") (Var "x")) $ Plus (Var "x") (Var "x"))
            [PushNum 13,StoreVar "x",PushNum 42,StoreVar "x",PushVar "x",PushVar "x",Add,StoreVar "x",PushVar "x",PushVar "x",Add]
        ]
    testCompile name inp out = testCase name $ compile inp @?= (out :: StackProgram String)


testsEval :: TestTree
testsEval =
  testGroup "Eval" [testPushNum, testPushVar, testAdd, testStoreVar, testExecProgram]
  where
    testPushNum = 
      testGroup "PushNum"
        [ testInstr "[] PushNum 1874 = [1874]"
            (PushNum 1874)
            initialState
            (MachineState [1874] M.empty, Right ()),
          testInstr "[42] PushNum 1874 = [1874, 42]"
            (PushNum 1874)
            (MachineState [42] M.empty)
            (MachineState [1874, 42] M.empty, Right ())
        ]
    testPushVar =
      testGroup "PushVar"
        [ let env = M.singleton "x" 1874 
            in testInstr "{x=1874} PushVar x = [1874]"
              (PushVar "x")
              (MachineState [] env)
              (MachineState [1874] env, Right ()),
          testInstr "{} PushVar x = VarUndefined x"
            (PushVar "x")
            initialState
            (initialState, Left (VarUndefined "x"))
        ]
    testAdd =
      testGroup "Add"
        [ testInstr "[3, 4] Add = [7]" Add
            (MachineState [3, 4] M.empty)
            (MachineState [7] M.empty, Right ()),
          testInstr "[3] Add = StackUnderflow" Add
            (MachineState [3] M.empty)
            (MachineState [3] M.empty, Left (StackUnderflow Add)),
          testInstr "[] Add = StackUnderflow" Add
            initialState
            (initialState, Left (StackUnderflow Add)),
          testInstr "[3, 4, 1874] Add = [7, 1874]" Add
            (MachineState [3, 4, 1874] M.empty)
            (MachineState [7, 1874] M.empty, Right ())
        ]
    testStoreVar =
      testGroup "StoreVar"
        [ testInstr "{} [1874] StoreVar x"
            (StoreVar "x")
            (MachineState [1874] M.empty)
            (MachineState [] (M.singleton "x" 1874), Right ()),
          testInstr "{x=42} [1874] StoreVar x"
            (StoreVar "x")
            (MachineState [1874] (M.singleton "x" 42))
            (MachineState [] (M.singleton "x" 1874), Right ()),
          testInstr "{} [] StoreVar x"
            (StoreVar "x")
            initialState
            (initialState, Left (StackUnderflow $ StoreVar "x"))
        ]
    testInstr name instr state res = testCase name $ runMyState (execInstr (instr :: StackInstr String)) state @?= res
    testExecProgram =
      testGroup "ExecProgram"
        [ testCase "[PushNum 1874] = [1874] {}"
            $ execProgram ([PushNum 1874] :: StackProgram String) initialState
            @?= Right (MachineState [1874] M.empty),
          testCase "[PushNum 3, PushNum 4, Add, StoreVar x, PushVar x] = [7] {x=7}"
            $ execProgram ([PushNum 3, PushNum 4, Add, StoreVar "x", PushVar "x"] :: StackProgram String) initialState
            @?= Right (MachineState [7] (M.singleton "x" 7)),
          testCase "Complex 1"
            $ execProgram ([PushNum 13, StoreVar "x", PushNum 42, StoreVar "y", PushVar "x", PushVar "y", Add, StoreVar "x", PushVar "x", PushVar "y", Add] :: StackProgram String) initialState
            @?= Right (MachineState [97] (M.fromList [("x", 55), ("y", 42)])),
          testCase "Complex 2"
            $ execProgram ([PushNum 13, StoreVar "x", PushNum 42, StoreVar "x", PushVar "x", PushVar "x", Add, StoreVar "x", PushVar "x", PushVar "x", Add] :: StackProgram String) initialState
            @?= Right (MachineState [168] (M.fromList [("x", 84)])),
          testCase "[Add] = StackUnderflow"
            $ execProgram ([Add] :: StackProgram String) initialState
            @?= Left (StackUnderflow Add),
          testCase "[PushNum 1, PushNum 2] = StackNotExhausted"
            $ execProgram ([PushNum 1, PushNum 2] :: StackProgram String) initialState
            @?= Left (StackNotExhausted [2, 1])
        ]

main :: IO ()
main =
  defaultMain $ testGroup "Tests" [testsCompile, testsEval]