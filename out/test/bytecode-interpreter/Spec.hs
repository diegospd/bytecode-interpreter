import Helpers.Machines
import Helpers.StackMachine
import StackMachine.Eval
import StackMachine.Sync.Eval
import Types.Bytecode
import Types.StackMachine

testsCommon :: [(Value, StackMachine)]
testsCommon =
  [ (Num 2, onePlusOne),
    (Num 4, twoTimesTwo),
    (Num 2, loadOnePlusOne),
    (Boolean True, oneMinusOneIsZero),
    (Boolean False, onePlusOneIsZero),
    (Num 1, writeReadOne),
    (Num 4, exampleMachine),
    (Num 1, oneWhenNotZero 1),
    (Num 0, oneWhenNotZero 0),
    (Num 1, loopReturnOne),
    (Num 0, loopAddOne 0),
    (Num 5, loopAddOne 5),
    (Boolean True, isEven 0),
    (Boolean False, isEven 7),
    (Boolean True, isEven 10),
    (Num 2, onePlusOne)
  ]


testsOnlySync :: [(Value, StackMachineSync)]
testsOnlySync = []



expectedAnswersSync :: [(Value, StackMachineSync)]
expectedAnswersSync =
 let applyToSecond = \f (a, b) -> (a, f b)
     commonTestsSync = map (applyToSecond realToSync) testsCommon
 in testsOnlySync ++ commonTestsSync 

test :: (Value, StackMachine) -> IO (Value, String)
test (expected, machine) = do
  result <- repl machine
  return (expected, if result == expected then "." else "FAILED actual: " <> show result)
  
testSync :: (Value, StackMachine) -> IO (Value, String)
testSync (expected, machine) = do
  result <- executeAsSync machine
  return (expected, if result == expected then "." else "FAILED actual: " <> show result)  
  
testingExpectedAnswers = mapM test testsCommon
testingExpectedAnswersSync = mapM testSync testsCommon

tester :: IO Bool
tester = all (("." ==) . snd) <$> testingExpectedAnswers

testerSync :: IO Bool
testerSync = all (("." ==) . snd) <$> testingExpectedAnswersSync

machineTest :: IO ()
machineTest = do
  putStrLn $ "\n\nTesting " <> show (length testsCommon) <> " machines..."
  passed <- tester
  if passed
    then putStrLn " ALL Passed!"
    else do putStrLn "NOT PASSED :("
            results <- testingExpectedAnswers
            putStrLn $ show results
  putStrLn "\n\n\n"

machineTestSync :: IO ()
machineTestSync = do
  putStrLn $ "\n\nSync testing " <> show (length expectedAnswersSync) <> " machines..."
  passed <- testerSync
  if passed
    then putStrLn " ALL Passed!"
    else do putStrLn "NOT PASSED :("
            results <- testingExpectedAnswersSync
            putStrLn $ show results
  putStrLn "\n\n\n"  
  
main :: IO ()
main = do
  machineTest
  machineTestSync


