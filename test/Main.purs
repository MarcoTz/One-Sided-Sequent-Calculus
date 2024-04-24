module Test.Main where

import Prelude (bind, pure,($), show,(<>), class Show, (+),(-))
import Data.Unit (Unit,unit)
import Data.Tuple (Tuple(..))
import Data.Either (Either(..))
import Data.Unfoldable (range)
import Data.Traversable (for)

import Effect (Effect)
import Effect.Class.Console (logShow)

import Syntax.Parsed.Program (Program)
import Driver.Driver (parseProg,inferAndRun)
import Driver.Definition (runDriverM,initialDriverState)
import Errors (showInSrc,getMessage)

import CounterExamples (getCex, numCex)
import ImportLibs (libSources)

data Example = CounterExample Int | Example String
instance Show Example where 
  show (CounterExample i) = "Counterexample " <> show (i+1)
  show (Example nm) = "Library " <> show nm


data TestRes = 
  TestSucc Example 
  | TestParserErr Example String
  | TestErr Example String

instance Show TestRes where 
  show (TestSucc ex) = "Test for " <> show ex <> " executed successfully"
  show (TestParserErr ex msg) = show ex <> " could not be parsed,\nerror: " <> msg
  show (TestErr ex msg) = "Test for " <> show ex <> " failed with message: " <> msg

parseExample :: Example -> String -> Either Program TestRes
parseExample ex src = do 
  let progParsed = runDriverM initialDriverState (parseProg src)
  case progParsed of 
    Tuple (Left err) _  -> Right $ TestParserErr ex (showInSrc err src) 
    Tuple (Right prog) _ -> Left prog

runCounterExamples :: Effect Unit
runCounterExamples = do
  let cexInds :: Array Int 
      cexInds = range 0 (numCex-1)
  cexStrs <- for cexInds (\i -> pure $ Tuple (CounterExample i) (getCex i))
  ress <- for cexStrs (\(Tuple ex src) -> pure $ runExample ex src true) 
  _ <- for ress (\res -> logShow res)
  pure unit

runExamples :: Effect Unit
runExamples = do
  ress <- for libSources (\(Tuple name src) -> pure $ runExample (Example name) src false)
  _ <- for ress (\res -> logShow res)
  pure unit

runExample :: Example -> String -> Boolean -> TestRes
runExample ex src shouldFail = do 
  let parseRes = parseExample ex src
  case parseRes of 
    Right res -> res 
    Left prog -> do 
      let progRes = runDriverM initialDriverState (inferAndRun prog)
      case progRes of 
          Tuple (Left err) _ -> if shouldFail then TestSucc ex else TestErr ex (getMessage err)
          Tuple (Right _) _ -> if shouldFail then TestErr ex "" else TestSucc ex 

main :: Effect Unit
main = do
  _ <- runCounterExamples 
  runExamples


