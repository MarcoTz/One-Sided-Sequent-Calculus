module Errors where 

import Common 
import Syntax.Typed.Types     qualified as T
import Syntax.Desugared.Terms qualified as D
import Syntax.Desugared.Types qualified as D

data Error =
   ErrXtorArity       !XtorName !ErrWhere
   | ErrBadPattern    ![XtorName] !ErrWhere
   | ErrTyArity       !TypeName !ErrWhere
   | ErrKind          !Kind !Kind !KindReason !ErrWhere
   | ErrTypeNeq       !T.Ty !T.Ty !ErrWhere
   | ErrNotTyDecl     !TypeName !T.Ty !ErrWhere
   | ErrNotTyShift    !T.Ty !ErrWhere
   | ErrTypeSchemeNeq !D.TypeScheme !D.TypeScheme !ErrWhere
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
