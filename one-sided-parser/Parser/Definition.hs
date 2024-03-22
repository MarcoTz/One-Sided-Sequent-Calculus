module Parser.Definition (
  Parser,
  runFileParser,
  ParseDecl (..),
) where 

import Errors
import Loc
import Syntax.Parsed.Program
import Syntax.Parsed.Terms

import Text.Megaparsec
import Control.Monad.Plus
import Control.Applicative (Alternative)

newtype Parser a = Parser { getParser :: Parsec String String  a }
  deriving newtype (Functor, Applicative, Monad, MonadFail, Alternative, MonadPlus, MonadParsec String String)

instance Error (ParseErrorBundle String String) where 
  getMessage _ = ""
  getLoc _ = defaultLoc
  toError = error "not implemented"

runFileParser :: FilePath -> Parser b -> String -> Either (ParseErrorBundle String String) b
runFileParser fp p = runParser (getParser p) fp 

data ParseDecl = MkD !DataDecl | MkV !VarDecl | MkA !AnnotDecl | MkI !Import | MkM !Command | MkR !RecDecl
