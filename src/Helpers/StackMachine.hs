module Helpers.StackMachine where

import Control.Monad.STM
import Data.Foldable (forM_)
import Data.HashMap as HashMap
import Lens.Micro.Platform
import Types.Bytecode
import Types.StackMachine

realizeBinding :: Map Identifier (STM a) -> IO (Map Identifier a)
realizeBinding environment = do
  let variableNames = keys environment
  results <- atomically <$> sequence $ elems environment
  return . HashMap.fromList $ zip variableNames results

realizeContext :: [Map Identifier (STM a)] -> IO [Map Identifier a]
realizeContext contextStack = sequence $ realizeBinding <$> contextStack

realToSync :: StackMachine -> StackMachineSync
realToSync machine =
  StackMachineSync
    { _bytecodeSync = machine ^. bytecode,
      _contextSync = fmap return <$> machine ^. context,
      _operandsSync = fmap return $ machine ^. operands,
      _debugSync = machine ^. debug
    }

syncToReal :: StackMachineSync -> IO StackMachine
syncToReal machine = do
  realizedOperands <- sequence $ atomically <$> machine ^. operandsSync
  realizedContext <- realizeContext $ machine ^. contextSync
  return
    StackMachine
      { _bytecode = machine ^. bytecodeSync,
        _context = realizedContext,
        _operands = realizedOperands,
        _debug = machine ^. debugSync
      }
