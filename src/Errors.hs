module Errors where 

import Common 
import Syntax.Typed.Types     qualified as T
import Syntax.Desugared.Types qualified as D

data Error =
   ErrXtorArity       !XtorName !ArityReason !ErrWhere
   | ErrBadPattern    ![XtorName] !ErrWhere
   | ErrTyArity       !TypeName !ErrWhere
   | ErrKind          !Kind !Kind !KindReason !ErrWhere
   | ErrTypeNeq       !T.Ty !T.Ty !ErrWhere
   | ErrTypeSchemeNeq !D.TypeScheme !D.TypeScheme !ErrWhere
   | ErrMissingDecl   !TypeName !ErrWhere
   | ErrMissingVar    !Variable !ErrWhere
   | ErrMissingXtor   !XtorName !ErrWhere
   | ErrMissingXtorPt !XtorName !ErrWhere
   | ErrParser        !String


data ArityReason = TooMany | TooFew
data KindReason = ShouldEq | ShouldNeq
data ErrWhere = WhereEval | WhereInfer | WhereSolve | WhereEnv | WhereDecl | WhereCheck | WhereParser | WhereGenConstr | WhereDesugar
