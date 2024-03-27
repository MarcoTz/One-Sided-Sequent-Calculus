module Errors (
  Error (..),
  zipWithError,
  convertError,
  showWithLoc,
  showInSrc
) where 

import Loc
import Control.Monad.Except

class Error a where 
  getMessage :: a -> String
  getLocation :: a -> Loc
  toError :: Loc -> String -> a

zipWithError :: Error e => MonadError e m => [a] -> [b] -> e -> m [(a,b)]
zipWithError [] [] _ = return []
zipWithError [] (_:_) err = throwError err
zipWithError (_:_) [] err = throwError err
zipWithError (a1:as) (b1:bs) err = (\z -> (a1,b1) : z) <$> zipWithError as bs err

convertError :: Error e => Error e' => e -> e'
convertError e = toError (getLocation e) (getMessage e) 

showWithLoc :: Error e => e -> String
showWithLoc e = getMessage e <> "\n" <> show (getLocation e)

showInSrc :: Error e => e -> String -> String
showInSrc e src = showLocInSource src (getLocation e) <> "\n" <> getMessage e
