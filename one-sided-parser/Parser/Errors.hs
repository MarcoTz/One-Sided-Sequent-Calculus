module Parser.Errors (
  ParserErr,
  bundleToErr
)where 

import Loc 
import Errors 

import Text.Megaparsec
import Data.List (intercalate)
import Data.List.NonEmpty qualified as NE 
import Data.Set qualified as S 

data ParserErr where
  ErrParser :: Loc -> String -> ParserErr 
  deriving (Eq,Ord)

instance Error ParserErr where 
  getMessage (ErrParser _ str) = str

  getLocation (ErrParser loc _) = loc 

  toError = ErrParser

instance ShowErrorComponent ParserErr where 
  showErrorComponent = getMessage

instance Show ParserErr where 
  show = getMessage

bundleToErr :: ParseErrorBundle String String -> ParserErr
bundleToErr (ParseErrorBundle errs pos) = do 
  let endOffset = errorOffset $ NE.last errs
  ErrParser (posStateToLoc pos endOffset) (intercalate "\n" (getErrorMessage <$> NE.toList errs))

posStateToLoc :: PosState String -> Int -> Loc 
posStateToLoc pos offset = 
  let (SourcePos _ line col) = pstateSourcePos pos 
      startPos = MkSourcePos (unPos line) (unPos col)
      endPos   = MkSourcePos (unPos line) (unPos col + offset)
  in MkLoc startPos endPos 

getErrorMessage :: ParseError String String -> String
getErrorMessage (TrivialError _ Nothing expSet) = "expected: " <> intercalate ", " (showItem <$> S.toList expSet)
getErrorMessage (TrivialError ofs (Just unexp) expTok) = "unexpected " <> showItem unexp <> ", " <> getErrorMessage (TrivialError ofs Nothing expTok)
getErrorMessage (FancyError _ errSet) = intercalate "\n" (showFancy <$> S.toList errSet)

showItem :: Show t => ErrorItem t -> String
showItem (Tokens tks) = intercalate "" (show <$> NE.toList tks)
showItem (Label lb) = NE.toList lb
showItem EndOfInput = "end of input"

showFancy :: ErrorFancy String -> String
showFancy (ErrorFail msg) = msg
showFancy (ErrorIndentation _ shouldInd actualInd) = "Wrong Indentation, should be " <> show shouldInd <> ", but found " <> show actualInd
showFancy (ErrorCustom msg) = msg
