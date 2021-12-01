module Eval.Operands where

import Control.Monad.STM
import Types.Bytecode
  
extractNum :: STM Value -> STM Int
extractNum syncValue = do
  value <- syncValue
  let (Num n) = value
  return n
  
extractBool :: STM Value -> STM Bool
extractBool syncValue = do
  value <- syncValue
  let (Boolean b) = value
  return b

