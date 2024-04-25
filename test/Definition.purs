module Test.Definition (
  Example (..),
  TestRes (..),
  runExample
)where

import Prelude (class Show,show,(<),(<>),(+),($),(-)) 
import Data.String (replaceAll, Pattern(..),Replacement(..),length)
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Data.Monoid (power)

import Common (Modulename)
import Errors (showInSrc,getMessage)
import Driver.Definition (runDriverM, initialDriverState)
import Driver.Driver (parseProg,inferAndRun) 
import Syntax.Parsed.Program (Program)

data Example = CounterExample Int | StdLib Modulename
instance Show Example where 
  show (CounterExample i) = "Counterexample " <> showNr (i+1)
    where 
      showNr :: Int -> String 
      showNr n | n<10 = "0" <> show n
      showNr n = show n 
  show (StdLib mn) = "Library " <> showMn mn
    where 
      showMn :: Modulename -> String
      showMn name = do
        let nm = show name
        let nr = length nm
        if nr < 6 then 
          nm <> power " " (6-nr)
        else nm


data TestRes = 
  TestSucc Example 
  | TestParserErr Example String
  | TestErr Example String

instance Show TestRes where 
  show (TestSucc ex)          = show ex <> ": Test executed successfully"
  show (TestParserErr ex msg) = "\nError: " <> show ex <> " could not be parsed,"<>
                                "\n\terror: " <> (replaceAll (Pattern "\n") (Replacement "\n\t") msg) <> "\n"
  show (TestErr ex msg)       = "\nError: " <> show ex <> " failed with message:" <> 
                                "\n\t" <> (replaceAll (Pattern "\n") (Replacement "\n\t") msg) <> "\n"


parseExample :: Example -> String -> Either Program TestRes
parseExample ex src = do 
  let progParsed = runDriverM initialDriverState (parseProg src)
  case progParsed of 
    Tuple (Left err) _  -> Right $ TestParserErr ex (showInSrc err src) 
    Tuple (Right prog) _ -> Left prog

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
