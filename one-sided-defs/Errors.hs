module Errors where 

import Common 
import Syntax.Parsed.Types
import Syntax.Parsed.Terms
import Control.Monad.Except

data Error =
   ErrXtorArity       !XtorName !String
   | ErrBadPattern    ![XtorName] !String
   | ErrTyArity       !TypeName !String
   | ErrKind          !KindReason !Ty !Ty !String
   | ErrTypeNeq       !Ty !Ty !String
   | ErrNotTyDecl     !TypeName !Ty !String
   | ErrNotTyShift    !Ty !String
   | ErrMissingDecl   !TypeName !String
   | ErrDuplDecl      !TypeName !String
   | ErrMissingVar    !Variable !String
   | ErrDuplVar       !Variable !String
   | ErrMissingTyVar  !TypeVar  !String
   | ErrMissingXtor   !XtorName !String
   | ErrDuplXtor      !XtorName !String
   | ErrMissingType   !String
   | ErrMissingXtorPt !XtorName !String
   | ErrTypeAmbig     !Term !String
   | ErrParser        !String
   | ErrModuleNotFound !Modulename !String
   | ErrDuplModule    !Modulename !String
   | ErrMutualRec     !Modulename !String
   | ErrTyNotAllowed  !Ty !String


data KindReason = ShouldEq | ShouldNeq

zipWithError :: MonadError Error m => [a] -> [b] -> Error -> m [(a,b)]
zipWithError [] [] _ = return []
zipWithError [] (_:_) err = throwError err
zipWithError (_:_) [] err = throwError err
zipWithError (a1:as) (b1:bs) err = (\z -> (a1,b1) : z) <$> zipWithError as bs err

fromMaybeWithError :: MonadError Error m => Maybe a -> Error -> m a 
fromMaybeWithError Nothing err = throwError err
fromMaybeWithError (Just a) _ = return a

allEqWithError :: MonadError Error m => Eq a => [a] -> Error -> m ()
allEqWithError [] _ = return ()
allEqWithError [_] _ = return () 
allEqWithError (a1:(a2:as)) err = if a1 == a2 then allEqWithError (a2:as) err else throwError err


