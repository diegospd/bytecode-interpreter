module Eval.Sync.Operator where

import Control.Monad.STM
import Control.Monad.Trans.State.Lazy hiding (state)
import Eval
import Eval.Operands
import Helpers.Bytecode
import Lens.Micro.Platform
import StackMachine.Sync.Stack
import Types.Bytecode
import Types.StackMachine

operatorResultSync :: STM StackMachineSync -> Int -> STM Value -> STM StackMachineSync
operatorResultSync stateSync arity returnValueSync = do
  state <- consumeBytecode stateSync
  let droppingOperands = popManyOperandsSync state arity
  return $ droppingOperands & operandsSync %~ (returnValueSync :)

instance EvalSync Operator where
  syncStep stateSync code = do
    state <- stateSync
    let operandStack = state ^. operandsSync
    let (arity, valueSync) = primitiveOperator code operandStack
    operatorResultSync stateSync arity valueSync

primitiveOperator :: Operator -> Stack (STM Value) -> (Int, STM Value)
primitiveOperator Multiply (syncA : syncB : _) =
  ( 2,
    do
      a <- extractNum syncA
      b <- extractNum syncB
      return . Num $ a * b
  )
primitiveOperator Add (syncA : syncB : _) =
  ( 2,
    do
      a <- extractNum syncA
      b <- extractNum syncB
      return . Num $ a + b
  )
primitiveOperator IsZero (syncA : _) =
  ( 1,
    do
      a <- extractNum syncA
      return . Boolean $ (0 == a)
  )
primitiveOperator Not (syncA : _) =
  ( 1,
    do
      b <- extractBool syncA
      return . Boolean . not $ b
  )
