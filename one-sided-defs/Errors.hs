module Errors (
  Error (..),
  zipWithError,
  liftEitherErrorList,
  liftMaybe,
  maybeToErr,
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

liftEitherErrorList :: Error e => [Either a e] -> ([a],[e])
liftEitherErrorList [] = ([],[])
liftEitherErrorList (Left a:eits) = let (as,es) = liftEitherErrorList eits in (a:as,es)
liftEitherErrorList (Right e:eits) = let (as,es) = liftEitherErrorList eits in (as,e:es)

liftMaybe :: Error e => MonadError e m => Maybe a -> e -> m a
liftMaybe Nothing err = throwError err 
liftMaybe (Just a) _ = return a

maybeToErr :: Error e => Maybe a -> e -> Either a e
maybeToErr Nothing err = Right err 
maybeToErr (Just a) _ = Left a 

convertError :: Error e => Error e' => e -> e'
convertError e = toError (getLocation e) (getMessage e) 

showWithLoc :: Error e => e -> String
showWithLoc e = getMessage e <> "\n" <> show (getLocation e)

showInSrc :: Error e => e -> String -> String
showInSrc e src = showLocInSource src (getLocation e) <> "\n" <> getMessage e
