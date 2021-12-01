# bytecode-interpreter
_An implementation of a concurrent stack machine capable of interpreting bytecode_

# How to build and test
 1. [Install The Haskell Tool Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/) a.k.a. `stack`
 2. Run `stack build` once to download all dependencies
 3. Run `stack test` to run the tests defined in `test/Spec.hs`
 4. Run `stack ghci` to load the project into the repl
 
## Stack Machine (bytecode, operands, context)
. The stack machine uses three different stacks to control the program execution:
 - **Bytecode stack** - The code to be executed by the machine. We evaluate the top bytecode in order to do a 
   state transition for the machine
 - **Operands stack** - A stack where functions may pop operands from and push their output value. When the machine
   reaches a final state (empty bytecode stack) the top value in the operands stack is returned as the output for the
   machine execution
  - **Context stack** - A stack of mappings from variable names to value bindings. Currently, the machine never pushes a 
  new frame onto the context stack, but it could be used to implement local scopes

## Undefined states and type errors
The operational semantics for the machine has some stuck states from which it's impossible to continue.
Currently, the implementation for the machine doesn't attempt to fail gracefully when reaching these states, but they
could be used to implement low level instructions like try/catch.
### Stuck states
- When the arity of a bytecode is greater than the amount of available values in the operand stack
- When the type of a value doesn't match with the type of the bytecode argument. E.g `If` expects a boolean operand, not an integer.
- When a bytecode is defined in `Types.Bytecode` but lacks implementation in `Eval.Operator`  
- When waiting for value to be delivered to a channel


## Bytecode
Bytecodes have three different constructors depending on the intention of the associated code
  - **Primitive operators** Pure functions that consumes and pushes values only from the operand stack
  - **Low level instructions** Instructions that modify the bytecode or context stack
  - **Sync operations** Operations dealing with concurrency like forking, sending and receiving through a channel

## Two machines
The project defines two stack machines in `Types.StackMachine`. 
The first one is only capable of simulating primitive operators and low level instructions. 
The `Sync` machine  is also able to perform Sync instructions. 

### StackMachineSync
 `StackMachineSync` is the machine capable of dealing with `Channel`s. 
 A `Channel` is considered a `Value`, and as such they might appear in the operand stack.
 But in order to allow for atomic composition of functions that uses Channels for input/output, we now
 store `STM Value` in the operand stack instead of simple `Value`s . 

## Adding a new primitive operator
The stack machine and bytecode is meant to be expanded with more primitive operations as it becomes necessary.
In order to add new primitive operator we only need to declare it in `Types.Bytecode`, and then add an implementation
in `Eval.Operator`
