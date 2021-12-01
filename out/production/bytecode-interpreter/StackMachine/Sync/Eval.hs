module StackMachine.Sync.Eval where

--import Eval.Sync

import Control.Monad as Monad
import Control.Monad.STM
import Control.Monad.State.Lazy as State hiding (get)
import Control.Monad.Trans.State.Lazy hiding (state)
import Data.Foldable (forM_)
import Eval
import Eval.Sync.Instruction
import Eval.Sync.Atomic
import Eval.Sync.Operator
import Helpers.StackMachine
import Lens.Micro.Platform
import StackMachine.Stack
import StackMachine.State
import StackMachine.Sync.Stack
import StackMachine.Sync.State
import Types.Bytecode
import Types.StackMachine
import Control.Concurrent

--machineForTopFunction :: StackMachineSync -> Sta 

syncFlow :: STM StackMachineSync -> IO (Either StackMachineSync StackMachineSync)
syncFlow stateSync = do
  state <- atomically stateSync
  let topBytecode = head $ state ^. bytecodeSync
  let
  if isFinalSync state
    then return . Right $ state
    else
      Left <$> case topBytecode of
        Primitive code -> atomically $ syncStep stateSync code
        LowLevel code -> atomically $ syncStep stateSync code
        Sync Spawn -> do 
          thread1 <- forkIO undefined
          thread2 <- forkIO undefined
          return  undefined
        Sync code -> atomically $ syncStep stateSync code

evalSyncFlow :: STM StackMachineSync -> IO StackMachineSync
evalSyncFlow stateSync = do
  newState <- syncFlow stateSync
  case newState of
    Right finalState -> return finalState
    Left nextState -> evalSyncFlow (return nextState)

executeSync :: StackMachineSync -> IO Value
executeSync machine = do
  finalState <- evalSyncFlow (return machine)
  finalValueSync finalState

executeAsSync :: StackMachine -> IO Value
executeAsSync = executeSync . realToSync

debugPrintSync :: StackMachineSync -> String -> IO ()
debugPrintSync machine message = do
  realizedMachine <- syncToReal machine
  Monad.when (machine ^. debugSync) $
    do
      putStrLn "-------------------------"
      putStrLn $ "Machine state:\n" <> show realizedMachine
      putStrLn message

executeAndReportSync :: StackMachineSync -> IO Value
executeAndReportSync machine = do
  debugPrintSync machine "Current state: "
  newState <- syncFlow $ return machine
  case newState of
    Right finalState -> do
      result <- finalValueSync finalState
      debugPrintSync finalState "Next state: "
      putStrLn $ "Machine reached a final state\nResult: " <> show result
      return result
    Left nextState -> do
      debugPrintSync nextState "Next state: "
      executeAndReportSync nextState
