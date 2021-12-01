module Eval.Sync.Instruction where

import Eval.Instruction
import Control.Monad.Trans.State.Lazy hiding (state)
import Eval
import StackMachine.Sync.Context
import StackMachine.State
import StackMachine.Sync.State
import StackMachine.Sync.Stack
import Types.Bytecode
import Types.StackMachine
import Control.Monad.STM
import Lens.Micro.Platform
import Eval.Operands  
import Helpers.Bytecode



jumpTrueBranchSync :: StackMachineSync -> STM StackMachineSync
jumpTrueBranchSync state = return $ modifyBytecodeStackSync state dropNextBody

jumpFalseBranchSync :: StackMachineSync -> STM StackMachineSync
jumpFalseBranchSync state = return $ modifyBytecodeStackSync state dropCurrentBody


ifSync' :: StackMachineSync -> Bool -> STM StackMachineSync
ifSync' state condition  = do
  if condition
    then jumpTrueBranchSync state
    else jumpFalseBranchSync state

ifSync :: STM StackMachineSync -> STM StackMachineSync
ifSync stateSync = do
  state <- consumeBytecode stateSync
  condition <- extractBool . head $ state ^. operandsSync
  ifSync' state condition



loopSync' :: Int -> StackMachineSync -> STM StackMachineSync
loopSync' times state = do
  let loopedBytecode = repeatCurrentBody times (state ^. bytecodeSync)
  return $ withNewBodySync state loopedBytecode


loopSync :: STM StackMachineSync -> STM StackMachineSync
loopSync stateSync = do
  state <- consumeBytecode stateSync
  times <- extractNum $ head $ state ^. operandsSync
  loopSync' times state


loadValSync :: STM StackMachineSync -> Value -> STM StackMachineSync
loadValSync stateSync value = do
  state <- consumeBytecode stateSync
  return $ pushOperandSync state value

writeVarSync :: STM StackMachineSync -> Identifier -> STM StackMachineSync
writeVarSync stateSync variableName = do
  state <- consumeBytecode stateSync
  let topOperandSync = head $ state ^. operandsSync
  let poppingOperand = state & operandsSync %~ tail
  return $ writeToContextSync poppingOperand variableName topOperandSync

readVarSync :: STM StackMachineSync -> Identifier -> STM StackMachineSync
readVarSync stateSync variableName = do
  state <- consumeBytecode stateSync
  return $ readVariableSync state variableName


instance EvalSync Instruction where
  syncStep stateSync Loop = loopSync stateSync
  syncStep stateSync If = ifSync stateSync
  syncStep stateSync (LoadVal value) = loadValSync stateSync value
  syncStep stateSync (WriteVar variableName) = writeVarSync stateSync variableName
  syncStep stateSync (ReadVar variableName) = readVarSync stateSync variableName
  syncStep stateSync ReturnValue = consumeBytecode stateSync
  




--
--  evalSync (ReadVar name) = do
--    binding <- readVariableSync name
--    case binding of
--      (Just value) -> instructionResultSync 0 (Just value)
--      Nothing -> error $ "ERROR variable not declared: " <> name
--
--  evalSync ReturnValue = unitSync 0
--
--
--
--




