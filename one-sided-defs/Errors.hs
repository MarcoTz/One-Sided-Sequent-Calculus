module Errors (
  Error (..),
  zipWithError
) where 

import Loc
import Control.Monad.Except

class Error a where 
  getMessage :: a -> String
  getLoc :: a -> Loc
  toError :: String -> Loc -> a


zipWithError :: Error e => MonadError e m => [a] -> [b] -> e -> m [(a,b)]
zipWithError [] [] _ = return []
zipWithError [] (_:_) err = throwError err
zipWithError (_:_) [] err = throwError err
zipWithError (a1:as) (b1:bs) err = (\z -> (a1,b1) : z) <$> zipWithError as bs err
