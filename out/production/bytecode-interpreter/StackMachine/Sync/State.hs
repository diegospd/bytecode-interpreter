module StackMachine.Sync.State where

import Control.Monad.STM
import Control.Monad.Trans.State.Lazy hiding (state)
import qualified Data.HashMap as HashMap
import Data.List (elemIndices)
import Lens.Micro.Platform
import Types.Bytecode
import Types.StackMachine
import StackMachine.State
import Helpers.StackMachine

initialStateSync :: Stack Bytecode -> StackMachineSync
initialStateSync bytecodeStack =
  StackMachineSync
    { _bytecodeSync = bytecodeStack,
      _contextSync = [HashMap.empty],
      _operandsSync = [],
      _debugSync = False
    }

isFinalSync :: StackMachineSync -> Bool
isFinalSync state = null (state ^. bytecodeSync)

finalValueSync :: StackMachineSync -> IO Value
finalValueSync state = atomically $ head $ state ^. operandsSync

withNewBodySync :: StackMachineSync -> Stack Bytecode -> StackMachineSync
withNewBodySync machine newBody = machine & bytecodeSync .~ newBody

modifyBytecodeStackSync' :: StackMachineSync -> (Stack Bytecode -> Stack Bytecode) -> StackMachineSync
modifyBytecodeStackSync' state f = state & bytecodeSync %~ f


modifyBytecodeStackSync :: StackMachineSync -> (Stack Bytecode -> Stack Bytecode) -> StackMachineSync
modifyBytecodeStackSync state f = withNewBodySync state (f $ state ^. bytecodeSync)

showMachineSync :: StackMachineSync -> IO String
showMachineSync machine = show <$> syncToReal machine

