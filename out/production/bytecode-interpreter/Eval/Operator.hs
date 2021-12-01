module Eval.Operator where

import StackMachine.State
import Eval
import Types.Bytecode
import Types.StackMachine
import Lens.Micro.Platform
import Eval.Operands
import Control.Monad.STM
import Control.Monad.Trans.State.Lazy hiding (state)


operatorResult :: Int -> Value -> (Int, Int, Value)
operatorResult arity value = (1, arity, value)

evalOperator :: Operator -> Stack Value -> (Int, Int, Value)
evalOperator Multiply ((Num a) : (Num b) : _) = operatorResult 2 $ Num $ a * b
evalOperator Add      ((Num a) : (Num b) : _) = operatorResult 2 $ Num $ a + b
evalOperator IsZero   ((Num 0) : _) = operatorResult 1 $ Boolean True
evalOperator IsZero   (_ : _) = operatorResult 1 $ Boolean False
evalOperator Not (Boolean b : _) = operatorResult 1 $ Boolean . not $ b
evalOperator op ops = error $ "Op: " <> show op <> "\nOps: " <> show ops

instance Eval Operator where
  eval code = do
    state <- get
    return $ evalOperator code (state ^. operands)



    
    
    


