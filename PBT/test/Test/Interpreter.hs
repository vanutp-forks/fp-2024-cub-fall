module Test.Interpreter (props) where

import qualified Compiler as C
import Data.Either (isRight)
import qualified Eval as E
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified StackMachine as S
import Test.Tasty
import Test.Tasty.Hedgehog
import Test.Utils

prop_runOk :: Property
prop_runOk = property $ do
  varNames <- forAll genVariableNames
  expr <- forAll $ genExpr varNames

  let initialState = getInitialState varNames
  let compiled = C.compile expr
  let result = E.execProgram compiled initialState
  assert (isRight result)

prop_undefinedVariable :: Property
prop_undefinedVariable = property $ do
  varNames <- forAll genVariableNames
  expr <- forAll $ genExpr varNames
  undefinedVariable <- forAll $ Gen.string (Range.constant 11 20) Gen.alpha

  let initialState = getInitialState varNames
  let compiled = C.compile expr ++ [S.PushVar undefinedVariable]
  let result = E.execProgram compiled initialState
  case result of
    Left (E.VarUndefined v) -> assert (v == undefinedVariable)
    _ -> failure

props :: [TestTree]
props =
  [ testProperty "Compiled program should run without errors" prop_runOk,
    testProperty "Trying to access undefined variable should result in error" prop_undefinedVariable
  ]