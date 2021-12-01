module StackMachine.State where

import Control.Monad.STM
import Control.Monad.Trans.State.Lazy hiding (state)
import qualified Data.HashMap as HashMap
import Data.List (elemIndices)
import Lens.Micro.Platform
import Types.Bytecode
import Types.StackMachine
import Helpers.StackMachine

machineStep :: Int -> Int -> a -> MachineFlow (Int, Int, STM a)
machineStep consumedBytecodes arity value = return (consumedBytecodes, arity, return value)

initialState :: Stack Bytecode -> StackMachine
initialState bytecodeStack =
  StackMachine
    { _bytecode = bytecodeStack,
      _context = [HashMap.empty],
      _operands = [],
      _debug = False
    }

isFinal :: StackMachine -> Bool
isFinal state = null (state ^. bytecode)



finalValue :: StackMachine -> Value
finalValue state = head $ state ^. operands



withNewBody :: StackMachine -> Stack Bytecode -> StackMachine
withNewBody machine newBody = machine & bytecode .~ newBody



lookupReturnValue' :: Stack Bytecode -> [Int]
lookupReturnValue' = elemIndices (LowLevel ReturnValue)

lookupReturnValue :: StackMachine -> [Int]
lookupReturnValue machine = lookupReturnValue' (machine ^. bytecode)

dropCurrentBody :: Stack Bytecode -> Stack Bytecode
dropCurrentBody bytecodeStack =
  let (firstReturn : _) = lookupReturnValue' bytecodeStack
   in drop (1 + firstReturn) bytecodeStack

dropNextBody :: Stack Bytecode -> Stack Bytecode
dropNextBody bytecodeStack =
  let (firstReturn : secondReturn : _) = lookupReturnValue' bytecodeStack
   in take (1 + firstReturn) bytecodeStack
        ++ drop (1 + secondReturn) bytecodeStack

extractCurrentBody ::  Stack Bytecode -> Stack Bytecode
extractCurrentBody bytecodeStack =
  let (firstReturn : _) = lookupReturnValue' bytecodeStack
      bodyLength = 1 + firstReturn
   in take bodyLength bytecodeStack

repeatCurrentBody :: Int -> Stack Bytecode -> Stack Bytecode
repeatCurrentBody n bytecodeStack =
  let (firstReturn : _) = lookupReturnValue' bytecodeStack
      bodyLength = 1 + firstReturn
   in concat (replicate n $ take bodyLength bytecodeStack) ++ drop bodyLength bytecodeStack

modifyBytecodeStack :: Stack Bytecode -> (Stack Bytecode -> Stack Bytecode) -> MachineFlow ()
modifyBytecodeStack bytecodeStack f = do
  state <- get
  let machineWithNewBytecode = withNewBody state (f bytecodeStack)
  put machineWithNewBytecode

goku :: StackMachineSync -> 

