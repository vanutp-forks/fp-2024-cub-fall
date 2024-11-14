module Test.Utils where

import qualified Expr as E
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified StackMachine as S
import qualified Eval as Ev
import qualified Data.Map as M

genInt :: Gen Int
genInt = Gen.int (Range.constant (-1000) 1000)

genVariableNames :: Gen [String]
genVariableNames = Gen.list (Range.constant 1 10) $ Gen.string (Range.constant 1 10) Gen.alpha

getInitialState :: [String] -> Ev.MachineState String
getInitialState varNames = Ev.MachineState [] $ M.fromList $ zip varNames [1 ..]

genExpr :: [String] -> Gen (E.Expr String)
genExpr varNames =
  Gen.recursive
    Gen.choice
    [ E.Num <$> genInt,
      E.Var <$> Gen.element varNames
    ]
    [ E.Plus <$> genExpr varNames <*> genExpr varNames,
      E.Let <$> Gen.element varNames <*> genExpr varNames <*> genExpr varNames
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
