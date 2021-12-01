module StackMachine.Sync.Stack where

import Control.Monad.STM
import Control.Monad.Trans.State.Lazy hiding (state)
import qualified Data.HashMap as HashMap
import Lens.Micro.Platform
import Types.Bytecode
import Types.StackMachine

--- Bytecode
popManyBytecodesSync :: Int -> MachineFlowSync [Bytecode]
popManyBytecodesSync n = do
  state <- get
  put $ state & bytecodeSync %~ drop n
  return . take n $ state ^. bytecodeSync

--- Operands
peekOperandsSync :: Int -> MachineFlowSync [STM Value]
peekOperandsSync n = do
  state <- get
  return $ take n $ state ^. operandsSync

peekOperandSync :: MachineFlowSync (STM Value)
peekOperandSync = head <$> peekOperandsSync 1

pushOperandSync :: StackMachineSync -> Value -> StackMachineSync
pushOperandSync state value = do
  state & operandsSync %~ (return value :)

popManyOperandsSync :: StackMachineSync -> Int -> StackMachineSync
popManyOperandsSync state n = state & operandsSync %~ drop n
