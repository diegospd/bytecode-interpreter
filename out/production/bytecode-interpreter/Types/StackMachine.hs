{-# LANGUAGE TemplateHaskell #-}

module Types.StackMachine where

import Control.Monad.STM
import Control.Monad.State.Lazy
import Control.Monad.Trans.State.Lazy
import Data.Functor.Identity
import Data.HashMap
import Lens.Micro.Platform
import Types.Bytecode

type Stack = []

type EnvironmentSync = Map Identifier (STM Value)
type Environment = Map Identifier Value

data StackMachine = StackMachine
  { _bytecode :: Stack Bytecode,
    _context :: Stack Environment,
    _operands :: Stack Value,
    _debug :: Bool
  }
  deriving (Show)

data StackMachineSync = StackMachineSync
  { _bytecodeSync :: Stack Bytecode,
    _contextSync :: Stack EnvironmentSync,
    _operandsSync :: Stack (STM Value),
    _debugSync :: Bool
  }


makeClassy ''StackMachine
makeClassy ''StackMachineSync

type MachineFlow a = State StackMachine a
type MachineFlowSync a = State StackMachineSync a

