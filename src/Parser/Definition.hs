module Parser.Definition where 


import Text.Megaparsec
import Control.Monad.Plus
import Data.Text qualified as T
import Control.Applicative (Alternative)


newtype Parser a = Parser { getParser :: Parsec String T.Text a }
  deriving newtype (Functor, Applicative, Monad, MonadFail, Alternative, MonadPlus, MonadParsec String T.Text)

runFileParser :: FilePath -> Parser b -> T.Text -> Either String b
runFileParser fp p input = case runParser (getParser p) fp input of 
  Left s -> Left (show s)
  Right x -> pure x 
