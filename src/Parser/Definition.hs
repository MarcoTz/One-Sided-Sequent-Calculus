module Parser.Definition where 

import Errors
import Syntax.Parsed.Program

import Text.Megaparsec
import Control.Monad.Plus
import Data.Text qualified as T
import Control.Applicative (Alternative)
import Control.Monad.Except

newtype Parser a = Parser { getParser :: (ParsecT String T.Text (Except Error)) a }
  deriving newtype (Functor, Applicative, Monad, MonadFail, Alternative, MonadPlus, MonadParsec String T.Text, MonadError Error)

runFileParser :: FilePath -> Parser b -> T.Text -> Either Error b
runFileParser fp p input = case runExcept (runParserT (getParser p) fp input) of 
  Left err -> Left err 
  Right (Left err) -> Left (ErrParser (show err))
  Right (Right x) -> Right x

data ParseDecl = MkD !DataDecl | MkV !VarDecl | MkA !AnnotDecl
