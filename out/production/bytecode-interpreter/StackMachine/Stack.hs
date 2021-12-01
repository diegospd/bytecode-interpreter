module StackMachine.Stack where

import Control.Monad.STM
import Control.Monad.Trans.State.Lazy hiding (state)
import qualified Data.HashMap as HashMap
import Lens.Micro.Platform
import Types.Bytecode
import Types.StackMachine

--- Bytecode
popManyBytecodes :: Int -> MachineFlow [Bytecode]
popManyBytecodes n = do
  state <- get
  put $ state & bytecode %~ drop n
  return . take n $ state ^. bytecode

popBytecode :: MachineFlow Bytecode
popBytecode = head <$> popManyBytecodes 1

--- Context
popContext :: MachineFlow Environment
popContext = do
  state <- get
  let current = head $ state ^. context
      newEnv = case state ^. context of
        [_] -> [HashMap.empty]
        (_ : bottom) -> bottom
  put $ state & context .~ newEnv
  return current

pushContext :: Environment -> MachineFlow ()
pushContext env = do
  state <- get
  put $ state & context %~ (env :)

pushEmptyContext :: MachineFlow ()
pushEmptyContext = pushContext HashMap.empty

--- Operands

peekOperands :: Int -> MachineFlow [Value]
peekOperands n = do
  state <- get
  return $ take n $ state ^. operands


peekOperand :: MachineFlow Value
peekOperand = head <$> peekOperands 1


pushOperand :: Value -> MachineFlow ()
pushOperand v = do
  state <- get
  put $ state & operands %~ (v :)

popManyOperands :: Int -> MachineFlow [Value]
popManyOperands n = do
  state <- get
  put $ state & operands %~ drop n
  return . take n $ state ^. operands



popOperand :: MachineFlow Value
popOperand = head <$> popManyOperands 1
