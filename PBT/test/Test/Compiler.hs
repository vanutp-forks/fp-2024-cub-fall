module Test.Compiler (props) where

import qualified Compiler as C
import Hedgehog
import Test.Tasty
import Test.Tasty.Hedgehog
import Test.Utils

prop_instructionCount :: Property
prop_instructionCount = property $ do
  varNames <- forAll genVariableNames
  expr <- forAll $ genExpr varNames
  let compiled = C.compile expr
  assert (getInstructionCountExpr expr AnyExpr == getInstructionCountInstr compiled AnyInstr)

prop_instructionCount2 :: Property
prop_instructionCount2 = property $ do
  varNames <- forAll genVariableNames
  expr <- forAll $ genExpr varNames
  let compiled = C.compile expr
  assert (getInstructionCountExpr expr NumType == getInstructionCountInstr compiled PushNumType)
  assert (getInstructionCountExpr expr VarType == getInstructionCountInstr compiled PushVarType)
  assert (getInstructionCountExpr expr PlusType == getInstructionCountInstr compiled AddType)
  assert (getInstructionCountExpr expr LetType == getInstructionCountInstr compiled StoreVarType)

props :: [TestTree]
props =
  [ testProperty "Instruction count in the compiled program should be the same as in expression" prop_instructionCount,
    testProperty "Count of each instruction in the compiled program should be the same as equivalent instruction in expression" prop_instructionCount2
  ]