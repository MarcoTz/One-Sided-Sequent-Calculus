module Parser.Definition (
  Parser,
  runFileParser,
  ParseDecl (..),
) where 

import Errors
import Syntax.Parsed.Program
import Syntax.Parsed.Terms

import Text.Megaparsec
import Control.Monad.Plus
import Control.Applicative (Alternative)

newtype Parser a = Parser { getParser :: Parsec String String  a }
  deriving newtype (Functor, Applicative, Monad, MonadFail, Alternative, MonadPlus, MonadParsec String String)

runFileParser :: FilePath -> Parser b -> String -> Either Error b
runFileParser fp p input = case runParser (getParser p) fp input of 
  Left err -> Left (ErrParser (show err))
  Right x -> Right x 

data ParseDecl = MkD !DataDecl | MkV !VarDecl | MkA !AnnotDecl | MkI !Import | MkM !Command | MkR !RecDecl
