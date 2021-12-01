module StackMachine.Context where

import Control.Monad.Trans.State.Lazy hiding (state)
import Data.HashMap as HashMap
import Lens.Micro.Platform
import StackMachine.Stack
import Types.Bytecode
import Types.StackMachine

currentContext :: MachineFlow Environment
currentContext = do
  state <- get
  return . unions $ state ^. context

readVariable :: Identifier -> MachineFlow (Maybe Value)
readVariable variableName = do
  env <- currentContext
  return $ HashMap.lookup variableName env

writeToContext :: Identifier -> Value -> MachineFlow ()
writeToContext variableName value = do
  state <- get
  let (current : contextTail) = state ^. context
  let newCurrent = insert variableName value current
  let newState = state & context .~ (newCurrent : contextTail)
  put newState

writeVariable :: Identifier -> Value -> MachineFlow ()
writeVariable variableName value = do
  env <- popContext
  pushContext $ insert variableName value env
