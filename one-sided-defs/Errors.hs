module Errors (
  Error (..),
  zipWithError,
  zipWithErrorM,
  convertError,
  showWithLoc,
  showInSrc
) where 

import Loc
import Control.Monad.Except
import Data.Bifunctor (first)

class Error a where 
  getMessage :: a -> String
  getLocation :: a -> Loc
  toError :: Loc -> String -> a

zipWithError :: Error e => [a] -> [b] -> e -> Either [(a,b)] e
zipWithError [] [] _ = Left []
zipWithError [] (_:_) err = Right err
zipWithError (_:_) [] err = Right err
zipWithError (a1:as) (b1:bs) err = do
  let rsZipped= zipWithError as bs err
  let leftFun ls = (a1,b1):ls
  first leftFun rsZipped

zipWithErrorM :: Error e => MonadError e m => [a] -> [b] -> e -> m [(a,b)]
zipWithErrorM as bs err = case zipWithError as bs err of 
  Left zipped -> return zipped 
  Right _ -> throwError err


convertError :: Error e => Error e' => e -> e'
convertError e = toError (getLocation e) (getMessage e) 

showWithLoc :: Error e => e -> String
showWithLoc e = getMessage e <> "\n" <> show (getLocation e)

showInSrc :: Error e => e -> String -> String
showInSrc e src = showLocInSource src (getLocation e) <> "\n" <> getMessage e
