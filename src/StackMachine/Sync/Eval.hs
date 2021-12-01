module StackMachine.Sync.Eval where

import Control.Concurrent
import Control.Monad as Monad
import Control.Monad.STM
import Control.Monad.State.Lazy as State hiding (get)
import Control.Monad.Trans.State.Lazy hiding (state)
import Data.Foldable (forM_)
import Eval
import Eval.Sync.Atomic
import Eval.Sync.Instruction
import Eval.Sync.Operator
import Helpers.StackMachine
import Lens.Micro.Platform
import StackMachine.State
import StackMachine.Sync.State
import Types.Bytecode
import Types.StackMachine

syncFlow :: STM StackMachineSync -> IO (Either StackMachineSync StackMachineSync)
syncFlow stateSync = do
  state <- atomically stateSync
  let topBytecode = head $ state ^. bytecodeSync
  let droppingTopBytecode = state & bytecodeSync %~ tail
  if isFinalSync state
    then return . Right $ state
    else
      Left <$> case topBytecode of
        Sync Spawn -> do
          let (forked1, forked2, current) = machinesForTopTwoFunctions droppingTopBytecode
          _ <- forkIO . void $ syncFlow (return forked1)
          _ <- forkIO . void $ syncFlow (return forked2)
          return current
        Sync code -> atomically $ syncStep stateSync code
        LowLevel code -> atomically $ syncStep stateSync code
        Primitive code -> atomically $ syncStep stateSync code

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
    do putStrLn $ message <> "\n" <> show realizedMachine <> "\n"

executeAndReportSync :: StackMachineSync -> IO Value
executeAndReportSync machine = do
  let debugMachine = machine & debugSync .~ True
  putStrLn "-------------------------"
  debugPrintSync debugMachine "Current state: "
  newState <- syncFlow $ return debugMachine
  case newState of
    Right finalState -> do
      result <- finalValueSync finalState
      debugPrintSync finalState "Next state: "
      putStrLn $ "=============== Machine reached a final state ===============\nResult: " <> show result
      return result
    Left nextState -> do
      debugPrintSync nextState "Next state: "
      executeAndReportSync nextState
