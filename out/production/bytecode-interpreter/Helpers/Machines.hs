module Helpers.Machines where

import Control.Monad.STM
import Data.HashMap as HashMap
import Helpers.Bytecode
import StackMachine.State (initialState)
import StackMachine.Sync.State (initialStateSync)
import Types.Bytecode
import Types.StackMachine

loadOnePlusOne :: StackMachine
loadOnePlusOne =
  initialState
    [ loadNumber 1,
      loadNumber 1,
      Primitive Add
    ]

onePlusOne :: StackMachine
onePlusOne =
  StackMachine
    { _bytecode = [Primitive Add],
      _context = [HashMap.empty],
      _operands = [Num 1, Num 1],
      _debug = False
    }

twoTimesTwo :: StackMachine
twoTimesTwo =
  StackMachine
    { _bytecode = [Primitive Multiply],
      _context = [HashMap.empty],
      _operands = [Num 2, Num 2],
      _debug = False
    }

oneMinusOneIsZero :: StackMachine
oneMinusOneIsZero =
  initialState
    [ loadNumber 1,
      loadNumber (-1),
      Primitive Add,
      Primitive IsZero
    ]

onePlusOneIsZero :: StackMachine
onePlusOneIsZero =
  initialState
    [ loadNumber 1,
      loadNumber 1,
      Primitive Add,
      Primitive IsZero
    ]

writeReadOne :: StackMachine
writeReadOne =
  initialState
    [ loadNumber 1,
      writeVar "x",
      readVar "x"
    ]

exampleMachine :: StackMachine
exampleMachine =
  initialState
    [ loadNumber 1,
      writeVar "x",
      loadNumber 2,
      writeVar "y",
      readVar "x",
      loadNumber 1,
      Primitive Add,
      readVar "y",
      Primitive Multiply,
      LowLevel ReturnValue
    ]

oneIfTrue :: StackMachine
oneIfTrue =
  StackMachine
    { _bytecode =
        [ LowLevel If,
          loadNumber 1,
          LowLevel ReturnValue,
          loadNumber 2,
          LowLevel ReturnValue
        ],
      _context = [HashMap.empty],
      _operands = [Boolean True],
      _debug = False
    }

oneWhenNotZero :: Int -> StackMachine
oneWhenNotZero n =
  initialState
    [ loadNumber n,
      Primitive IsZero,
      LowLevel If,
      loadNumber 0,
      LowLevel ReturnValue,
      loadNumber 1,
      LowLevel ReturnValue
    ]

loopReturnOne :: StackMachine
loopReturnOne =
  initialState
    [ loadNumber 3,
      LowLevel Loop,
      loadNumber 1,
      LowLevel ReturnValue
    ]

loopAddOne :: Int -> StackMachine
loopAddOne n =
  initialState
    [ loadNumber 0,
      writeVar "x",
      loadNumber n,
      LowLevel Loop,
      readVar "x",
      loadNumber 1,
      Primitive Add,
      writeVar "x",
      LowLevel ReturnValue,
      readVar "x"
    ]

isEven :: Int -> StackMachine
isEven n =
  initialState
    [ loadTrue,
      writeVar "x",
      loadNumber n,
      LowLevel Loop,
      readVar "x",
      Primitive Not,
      writeVar "x",
      LowLevel ReturnValue,
      readVar "x"
    ]

writeReadOneChan :: STM StackMachineSync
writeReadOneChan = do
  loadEmptyChan <- loadEmptyChannel
  return $
    initialStateSync
      [loadNumber 1,
      loadEmptyChan,
      Sync SendChannel]
