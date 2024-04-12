module Errors (
  class Error,
  getMessage,
  getLocation,
  toError,
  zipWithError,
  zipWithErrorM,
  convertError,
  showWithLoc,
  showInSrc
) where 

import Loc (Loc, showLocInSource)

import Prelude (pure, (<>), show)
import Data.List (List (..))
import Data.Either (Either (..))
import Data.Tuple (Tuple (..))

import Control.Monad.Except
import Data.Bifunctor (lmap)

class Error a where 
  getMessage :: a -> String
  getLocation :: a -> Loc
  toError :: Loc -> String -> a

zipWithError :: forall e a b.Error e => List a -> List b -> e -> Either (List (Tuple a b)) e
zipWithError Nil Nil _ = Left Nil
zipWithError Nil (Cons _ _) err = Right err
zipWithError (Cons _ _) Nil err = Right err
zipWithError (Cons a1 as) (Cons b1 bs) err = do
  let rsZipped= zipWithError as bs err
  let leftFun ls = Cons (Tuple a1 b1) ls
  lmap leftFun rsZipped

zipWithErrorM :: forall e m a b.Error e => MonadError e m => List a -> List b -> e -> m (List (Tuple a b))
zipWithErrorM as bs err = case zipWithError as bs err of 
  Left zipped -> pure zipped 
  Right _ -> throwError err


convertError :: forall e e'.Error e => Error e' => e -> e'
convertError e = toError (getLocation e) (getMessage e) 

showWithLoc :: forall e. Error e => e -> String
showWithLoc e = getMessage e <> "\n" <> show (getLocation e)

showInSrc :: forall e.Error e => e -> String -> String
showInSrc e src = showLocInSource src (getLocation e) <> "\n" <> getMessage e
