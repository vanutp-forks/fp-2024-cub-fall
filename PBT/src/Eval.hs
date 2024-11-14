module Eval where

import qualified Data.Map as M
import State
import StackMachine

-- Possible errors during evaluation
data Error v
  = StackUnderflow (StackInstr v) -- Not enough numbers on the stack to process the instruction
  | VarUndefined v                -- The variable is not defined in the environment 
  | StackNotExhausted Stack       -- After the program has finished evaluation, the stack is not a singleton
  deriving (Show, Eq)

type Stack = [Int] -- The stack, holding integers

type Env v = M.Map v Int -- The environment, mapping variables to their values

-- The machine state consists of the stack and the variable environment.
data MachineState v = MachineState
  { getStack :: Stack,
    getEnv :: Env v
  }
  deriving (Show, Eq)

-- Run the compiled program on an empty stack and environment
initialState :: MachineState String
initialState = MachineState [] M.empty

-- Execute a single instruction. 
-- Successful evaluation does not produce any useful result: only the effect of modifying state matters. 
execInstr :: (Ord v, Show v) => StackInstr v -> MyState (MachineState v) (Either (Error v) ())
execInstr (PushNum x) = fmap Right $ modify $ \s -> s { getStack = x : getStack s }
execInstr (PushVar k) = do
  env <- gets getEnv
  case M.lookup k env of
    Just v -> fmap Right $ modify $ \s -> s { getStack = v : getStack s }
    Nothing -> return $ Left (VarUndefined k)
execInstr Add = do
  stack <- gets getStack
  case stack of
    (a:b:rem) -> fmap Right $ modify $ \s -> s { getStack = a + b : rem}
    _ -> return $ Left (StackUnderflow Add)
execInstr (StoreVar k) = do
  stack <- gets getStack
  case stack of
    (v:rem) -> fmap Right $ modify $ \s -> s { getEnv = M.insert k v (getEnv s), getStack = rem }
    _ -> return $ Left (StackUnderflow $ StoreVar k)

-- Execute a list of instructions starting from the given state. 
execProgram :: (Ord v, Show v) => StackProgram v -> MachineState v -> Either (Error v) (MachineState v)
execProgram program state =
  let execProgram' [] state =
        case getStack state of
          [res] -> Right state
          _ -> Left (StackNotExhausted $ getStack state)
      execProgram' (instr:rem) state = 
        let (newState, res) = runMyState (execInstr instr) state
        in case res of
          Left err -> Left err
          Right _ -> execProgram' rem newState
  in execProgram' program state
