module Errors where 

import Common 
import Syntax.Parsed.Types    qualified as P
import Syntax.Typed.Types     qualified as T
import Syntax.Desugared.Terms qualified as D

import Control.Monad.Except

data Error =
   ErrXtorArity       !XtorName !ErrWhere
   | ErrBadPattern    ![XtorName] !ErrWhere
   | ErrTyArity       !TypeName !ErrWhere
   | ErrKind          !KindReason !ErrWhere
   | ErrTypeNeq       !P.Ty !P.Ty !ErrWhere
   | ErrNotTyDecl     !TypeName !T.Ty !ErrWhere
   | ErrNotTyShift    !T.Ty !ErrWhere
   | ErrMissingDecl   !TypeName !ErrWhere
   | ErrDuplDecl      !TypeName !ErrWhere
   | ErrMissingVar    !Variable !ErrWhere
   | ErrDuplVar       !Variable !ErrWhere
   | ErrMissingTyVar  !TypeVar  !ErrWhere
   | ErrMissingXtor   !XtorName !ErrWhere
   | ErrDuplXtor      !XtorName !ErrWhere
   | ErrMissingXtorPt !XtorName !ErrWhere
   | ErrTypeAmbig     !D.Term !ErrWhere
   | ErrParser        !String


data KindReason = ShouldEq | ShouldNeq
data ErrWhere = WhereEval | WhereInfer | WhereSolve | WhereEnv | WhereDecl | WhereCheck | WhereParser | WhereGenConstr | WhereDesugar

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


