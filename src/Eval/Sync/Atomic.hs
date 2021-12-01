module Eval.Sync.Atomic where

import Control.Concurrent.STM.TMVar
import Control.Monad.STM
import Eval
import Eval.Operands
import Lens.Micro.Platform
import Types.Bytecode
import Types.StackMachine

sendChannel' :: StackMachineSync -> Stack (STM Value) -> STM StackMachineSync
sendChannel' state (syncA : syncB : operandStack) = do
  channel <- extractChannel syncA
  value <- syncB
  putTMVar channel value
  return $ state & operandsSync .~ operandStack

sendChannel :: STM StackMachineSync -> STM StackMachineSync
sendChannel stateSync = do
  state <- stateSync
  sendChannel' state (state ^. operandsSync)

recoverChannel' :: StackMachineSync -> Stack (STM Value) -> STM StackMachineSync
recoverChannel' state (syncA : operandStack) = do
  channel <- extractChannel syncA
  return $ state & operandsSync .~ (takeTMVar channel : operandStack)

recoverChannel :: STM StackMachineSync -> STM StackMachineSync
recoverChannel stateSync = do
  state <- stateSync
  recoverChannel' state (state ^. operandsSync)

instance EvalSync Atomic where
  syncStep stateSync SendChannel = sendChannel stateSync
  syncStep stateSync RecoverChannel = undefined
