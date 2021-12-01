module Eval where

import Control.Monad.STM
import Types.Bytecode
import Types.StackMachine  
  
class Eval code where
  -- | Implementations of eval should not pop their operands from the stack.
  -- | They should peek at the required operands, and return a pair (arity, value)
  -- | This way we may control when we pop operands based on the output value 
  eval :: code -> MachineFlow (Int, Int, Value)


class EvalSync code where 
  syncStep :: STM StackMachineSync -> code -> STM StackMachineSync