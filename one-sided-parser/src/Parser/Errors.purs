module Parser.Errors (
  ParserErr,
  parseErrorToParserErr 
)where 

import Errors (class Error, getMessage)
import Loc (Loc, defaultLoc)

import Prelude (class Eq, class Ord, class Show)
import Parsing (ParseError(..))

data ParserErr = 
  ErrParser Loc String 
derive instance eqParserErr :: Eq ParserErr
derive instance ordParserErr :: Ord ParserErr

instance Error ParserErr where 
  getMessage (ErrParser _ str) = str

  getLocation (ErrParser loc _) = loc 

  toError = ErrParser


instance Show ParserErr where 
  show = getMessage

parseErrorToParserErr :: ParseError -> ParserErr
parseErrorToParserErr (ParseError msg _pos) = ErrParser defaultLoc msg
