module Eval.Operands where

import Control.Concurrent.STM.TMVar
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

extractChannel :: STM Value -> STM (TMVar Value)
extractChannel syncValue = do
  value <- syncValue
  let (Channel tmvar) = value
  return tmvar
