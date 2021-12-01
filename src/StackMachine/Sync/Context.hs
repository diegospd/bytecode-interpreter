module StackMachine.Sync.Context where

import Control.Monad.STM
import Control.Monad.Trans.State.Lazy hiding (state)
import Data.HashMap as HashMap
import Lens.Micro.Platform
import Types.Bytecode
import Types.StackMachine

currentContextSync :: MachineFlowSync EnvironmentSync
currentContextSync = do
  state <- get
  return . unions $ state ^. contextSync

writeToContextSync :: StackMachineSync -> Identifier -> STM Value -> StackMachineSync
writeToContextSync state variableName valueSync =
  let (current : contextTail) = state ^. contextSync
      newCurrent = insert variableName valueSync current
   in state & contextSync .~ (newCurrent : contextTail)

readVariableSync :: StackMachineSync -> Identifier -> StackMachineSync
readVariableSync state variableName =
  let currentContext = head $ state ^. contextSync
      (Just bindingSync) = HashMap.lookup variableName currentContext
   in state & operandsSync %~ (bindingSync :)
