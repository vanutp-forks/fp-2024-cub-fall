module Test.Props (props) where

import qualified Compiler as C
import qualified Expr as E
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified StackMachine as S
import Test.Tasty
import Test.Tasty.Hedgehog
import Test.Utils

genInt :: Gen Int
genInt = Gen.int (Range.constant (-1000) 1000)

genName :: Gen String
genName = Gen.string (Range.constant 1 10) Gen.alpha

genExpr :: Gen (E.Expr String)
genExpr =
  Gen.recursive
    Gen.choice
    [ E.Num <$> genInt,
      E.Var <$> genName
    ]
    [ E.Let <$> genName <*> genExpr <*> genExpr,
      E.Plus <$> genExpr <*> genExpr
    ]

data ExprType = NumType | VarType | PlusType | LetType | AnyExpr

data StackInstrType = PushNumType | PushVarType | AddType | StoreVarType | AnyInstr

isOfType :: E.Expr String -> ExprType -> Bool
isOfType (E.Num _) NumType = True
isOfType (E.Var _) VarType = True
isOfType (E.Plus _ _) PlusType = True
isOfType (E.Let _ _ _) LetType = True
isOfType _ AnyExpr = True
isOfType _ _ = False

isOfTypeInstr :: S.StackInstr String -> StackInstrType -> Bool
isOfTypeInstr (S.PushNum _) PushNumType = True
isOfTypeInstr (S.PushVar _) PushVarType = True
isOfTypeInstr S.Add AddType = True
isOfTypeInstr (S.StoreVar _) StoreVarType = True
isOfTypeInstr _ AnyInstr = True
isOfTypeInstr _ _ = False

getInstructionCountExpr :: E.Expr String -> ExprType -> Int
getInstructionCountExpr e@(E.Num _) t = fromEnum (isOfType e t)
getInstructionCountExpr e@(E.Var _) t = fromEnum (isOfType e t)
getInstructionCountExpr e@(E.Plus a b) t = fromEnum (isOfType e t) + getInstructionCountExpr a t + getInstructionCountExpr b t
getInstructionCountExpr e@(E.Let _ v b) t = fromEnum (isOfType e t) + getInstructionCountExpr v t + getInstructionCountExpr b t

getInstructionCountInstr :: [S.StackInstr String] -> StackInstrType -> Int
getInstructionCountInstr [] _ = 0
getInstructionCountInstr (i : is) t = fromEnum (isOfTypeInstr i t) + getInstructionCountInstr is t

prop_instructionCount :: Property
prop_instructionCount = property $ do
  expr <- forAll genExpr
  let compiled = C.compile expr
  assert (getInstructionCountExpr expr AnyExpr == getInstructionCountInstr compiled AnyInstr)

prop_instructionCount2 :: Property
prop_instructionCount2 = property $ do
  expr <- forAll genExpr
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