module StackMachine.Eval where

import Control.Monad as Monad
import Control.Monad.State.Lazy as State hiding (get)
import Control.Monad.Trans.State.Lazy hiding (state)
import Data.Foldable (forM_)
import Eval
import Eval.Instruction
import Eval.Operator
import Lens.Micro.Platform
import StackMachine.Stack
import StackMachine.State
import Types.Bytecode
import Types.StackMachine

evalBytecode :: MachineFlow ()
evalBytecode = do
  state <- get
  (consumedBytecodes, arity, value) <- case head $ state ^. bytecode of
    (Primitive code) -> eval code
    (LowLevel code) -> eval code
    (Sync _) -> error "MachineFlow doesn't support sync operations; use MachineFlowSync instead"
  _ <- popManyBytecodes consumedBytecodes
  _ <- popManyOperands arity
  case value of
    Unit -> return ()
    _ -> pushOperand value

executionStep :: StackMachine -> (Maybe Value, StackMachine)
executionStep state = do
  let (_, newState) = runState evalBytecode state
  if isFinal newState
    then (Just . finalValue $ newState, newState)
    else (Nothing, newState)

debugPrint :: StackMachine -> String -> IO ()
debugPrint state s = do
  Monad.when (state ^. debug) $
    do
      putStrLn "-------------------------"
      putStrLn $ "Machine state:\n" <> show state
      putStrLn s

repl :: StackMachine -> IO Value
repl state = do
  let (output, newState) = executionStep state
  debugPrint state $ "New state:\n" <> show newState
  case output of
    Nothing -> repl newState
    Just value -> do
      debugPrint newState $ "Evaluation Finished!\n=====================\nFinal result:\n" <> show newState
      return value

executeAndReport :: StackMachine -> IO Value
executeAndReport machine = do
  let debugMachine = machine & debug .~ True
  putStrLn "Simulating bytecode execution..."
  repl debugMachine
