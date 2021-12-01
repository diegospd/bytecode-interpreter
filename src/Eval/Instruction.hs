module Eval.Instruction where

import Control.Monad.Trans.State.Lazy hiding (state)
import Eval
import Lens.Micro.Platform
import StackMachine.Context
import StackMachine.Stack
import StackMachine.State
import Types.Bytecode
import Types.StackMachine

instructionResult :: Int -> Value -> MachineFlow (Int, Int, Value)
instructionResult arity value = return (1, arity, value)

unit :: Int -> MachineFlow (Int, Int, Value)
unit arity = instructionResult arity Unit

jumpTrueBranch :: Stack Bytecode -> MachineFlow ()
jumpTrueBranch bytecodeStack = modifyBytecodeStack bytecodeStack dropNextBody

jumpFalseBranch :: Stack Bytecode -> MachineFlow ()
jumpFalseBranch bytecodeStack = modifyBytecodeStack bytecodeStack dropCurrentBody

instance Eval Instruction where
  eval If = do
    condition <- peekOperand
    state <- get
    let droppingIfFrame = tail (state ^. bytecode)
    case condition of
      Boolean True -> do
        jumpTrueBranch droppingIfFrame
        return (0, 1, Unit)
      Boolean False -> do
        jumpFalseBranch droppingIfFrame
        return (0, 1, Unit)
        
  eval Loop = do
    value <- peekOperand
    let (Num n) = value
    state <- get
    let droppingLoop = tail (state ^. bytecode)
    modifyBytecodeStack droppingLoop (repeatCurrentBody n)
    return (0, 1, Unit)
    
  eval (LoadVal value) = do
    pushOperand value
    unit 0
    
  eval (WriteVar name) = do
    value <- peekOperand
    writeToContext name value
    unit 1
    
  eval (ReadVar name) = do
    binding <- readVariable name
    case binding of
      (Just value) -> instructionResult 0 value
      Nothing -> error $ "ERROR variable not declared: " <> name
  eval ReturnValue = unit 0
