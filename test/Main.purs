module Test.Main where

import Prelude (bind, pure,($), show,(<>), class Show, (+),(-))
import Data.Unit (Unit,unit)
import Data.Tuple (Tuple(..))
import Data.Either (Either(..))
import Data.Unfoldable (range)
import Data.Traversable (for)

import Effect (Effect)
import Effect.Class.Console (logShow)

import Driver.Driver (parseProg,inferAndRun)
import Driver.Definition (runDriverM,initialDriverState)
import Errors (getMessage)

import CounterExamples (getCex, numCex)

cexPrefix :: Int -> String
cexPrefix i = "Counterexample " <> show (i+1)

data TestRes = 
  TestSucc Int 
  | TestParserErr Int String
  | TestErr Int

instance Show TestRes where 
  show (TestSucc i) = cexPrefix i <> " failed successfully"
  show (TestParserErr i msg) = cexPrefix i <> " could not be parsed,\nerror: " <> msg
  show (TestErr i) = cexPrefix i <> " did not fail"

runCounterExample :: Int -> String -> TestRes 
runCounterExample i src = do 
  let progParsed = runDriverM initialDriverState (parseProg src)
  case progParsed of 
    Tuple (Left err) _  -> TestParserErr i ("Error Parsing: " <> getMessage err) 
    Tuple (Right prog) _ -> do
      let progRes = runDriverM initialDriverState (inferAndRun prog)
      case progRes of 
          Tuple (Left _) _ -> TestSucc i
          Tuple (Right _) _ -> TestErr i

main :: Effect Unit
main = do
  let cexInds :: Array Int 
      cexInds = range 0 (numCex-1)
  cexStrs <- for cexInds (\i -> pure $ Tuple i (getCex i))
  ress <- for cexStrs (\(Tuple i src) -> pure $ runCounterExample i src) 
  _ <- for ress (\res -> logShow res)
  pure unit
