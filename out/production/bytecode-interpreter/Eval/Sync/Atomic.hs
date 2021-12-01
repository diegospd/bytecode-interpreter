module Eval.Sync.Atomic where

  
import Types.Bytecode
import Types.StackMachine
import Eval

instance EvalSync Atomic where
  syncStep stateSync SendChannel = undefined
  syncStep stateSync RecoverChannel = undefined

