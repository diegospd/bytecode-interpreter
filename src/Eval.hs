module Eval where

import Control.Monad.STM
import Types.Bytecode
import Types.StackMachine

class Eval code where
  eval :: code -> MachineFlow (Int, Int, Value)

class EvalSync code where
  syncStep :: STM StackMachineSync -> code -> STM StackMachineSync
