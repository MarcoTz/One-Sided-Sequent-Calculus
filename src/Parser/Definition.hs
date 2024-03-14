module Parser.Definition where 

import Errors
import Syntax.Parsed.Program
import Syntax.Parsed.Terms

import Text.Megaparsec
import Control.Monad.Plus
import Data.Text qualified as T
import Control.Applicative (Alternative)

newtype Parser a = Parser { getParser :: Parsec String T.Text  a }
  deriving newtype (Functor, Applicative, Monad, MonadFail, Alternative, MonadPlus, MonadParsec String T.Text)

runFileParser :: FilePath -> Parser b -> T.Text -> Either Error b
runFileParser fp p input = case runParser (getParser p) fp input of 
  Left err -> Left (ErrParser (show err))
  Right x -> Right x 

data ParseDecl = MkD !DataDecl | MkV !VarDecl | MkA !AnnotDecl | MkI !Import | MkM !Command
