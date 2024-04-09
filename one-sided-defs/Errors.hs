module Errors (
  Error (..),
  zipWithError,
  liftEitherErrorList,
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

liftEitherErrorList :: Error e => [Either a e] -> ([a],[e])
liftEitherErrorList [] = ([],[])
liftEitherErrorList (Left a:eits) = let (as,es) = liftEitherErrorList eits in (a:as,es)
liftEitherErrorList (Right e:eits) = let (as,es) = liftEitherErrorList eits in (as,e:es)

convertError :: Error e => Error e' => e -> e'
convertError e = toError (getLocation e) (getMessage e) 

showWithLoc :: Error e => e -> String
showWithLoc e = getMessage e <> "\n" <> show (getLocation e)

showInSrc :: Error e => e -> String -> String
showInSrc e src = showLocInSource src (getLocation e) <> "\n" <> getMessage e
