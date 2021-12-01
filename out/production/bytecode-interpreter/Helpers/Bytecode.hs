module Helpers.Bytecode where

import Types.Bytecode
import Types.StackMachine
import Control.Monad.STM
import Lens.Micro.Platform
import Control.Concurrent.STM.TMVar

loadNumber :: Int -> Bytecode
loadNumber = LowLevel . LoadVal . Num

loadTrue :: Bytecode
loadTrue = LowLevel . LoadVal . Boolean $ True

loadFalse :: Bytecode
loadFalse = LowLevel . LoadVal . Boolean $ False

loadEmptyChannel :: STM Bytecode
loadEmptyChannel = do
  emptyChannel <- newEmptyTMVar
  let channelValue = Channel emptyChannel
  return . LowLevel . LoadVal $ channelValue

readVar :: Identifier -> Bytecode
readVar = LowLevel . ReadVar

writeVar :: Identifier -> Bytecode
writeVar = LowLevel . WriteVar

true :: Value -> Bool
true (Boolean True) = True
true _ = False




consumeBytecode :: STM StackMachineSync -> STM StackMachineSync
consumeBytecode stateSync = do
  state <- stateSync
  return $ state & bytecodeSync %~ tail
