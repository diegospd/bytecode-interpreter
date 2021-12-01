module Types.Bytecode where

import Control.Concurrent.STM.TMVar
import Control.Monad.STM

data Value
  = Unit
  | Num Int
  | Boolean Bool
  | Channel (TMVar Value)
  deriving (Eq)

data Operator
  = Multiply
  | Add
  | IsZero
  | Not
  deriving (Eq, Show)

type Identifier = String

data Instruction
  = If 
  | Loop
  | LoadVal Value
  | WriteVar Identifier
  | ReadVar Identifier
  | ReturnValue
  deriving (Eq, Show)

data Atomic 
  = SendChannel
  | RecoverChannel
  | Spawn
  deriving (Eq,Show)

data Bytecode
  = Primitive Operator
  | LowLevel Instruction
  | Sync Atomic
  deriving (Eq)

instance Show Value where
  show Unit = "()"
  show (Num n) = show n
  show (Boolean b) = show b
  show (Channel _) = "CHAN"

instance Show Bytecode where
  show (Primitive code) = show code
  show (LowLevel code) = show code
  show (Sync code) = "[[ " <> show code <> " ]]"

instance Show (STM a) where 
  show _ = "<pending value>"