module Parser.Definition (
  Parser,
  runSourceParser,
  ParseDecl (..),
) where 

import Common
import Syntax.Parsed.Program
import Syntax.Parsed.Terms
import Parser.Errors 

import Text.Megaparsec
import Control.Monad.Plus
import Control.Applicative (Alternative)

newtype Parser a = Parser { getParser :: Parsec String String  a }
  deriving newtype (Functor, Applicative, Monad, MonadFail, Alternative, MonadPlus, MonadParsec String String)


runSourceParser :: String -> Modulename -> Parser b -> Either ParserErr b
runSourceParser src (Modulename srcName) p = case runParser (getParser p) srcName src of 
  Left bundle -> Left $ bundleToErr bundle
  Right b -> Right b

data ParseDecl = MkD !DataDecl | MkV !VarDecl | MkA !AnnotDecl | MkI !Import | MkM !Command | MkR !RecDecl
