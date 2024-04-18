module Parser.Definition (
  SrcParser,
  runSourceParser,
  ParseDecl (..)
) where 

import Syntax.Parsed.Program (DataDecl, VarDecl, AnnotDecl, Import, RecDecl)
import Syntax.Parsed.Terms (Command)

import Parser.Errors (ParserErr,parseErrorToParserErr)

import Prelude (($))
import Parsing (Parser, runParser)
import Data.Either (Either(..))

type SrcParser a = Parser String a


runSourceParser :: forall a. String -> SrcParser a -> Either ParserErr a
runSourceParser src p = case runParser src p of 
  Left err -> Left $ parseErrorToParserErr  err
  Right b -> Right b

data ParseDecl = MkD DataDecl | MkV VarDecl | MkA AnnotDecl | MkI Import | MkM Command | MkR RecDecl
