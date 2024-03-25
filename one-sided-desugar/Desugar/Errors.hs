module Desugar.Errors (DesugarError (..)) where 

import Common 
import Loc
import Errors
import Syntax.Desugared.Types 
import Pretty.Common ()
import Pretty.Desugared ()

data DesugarError where 
  ErrVariable      :: Loc -> Variable -> DesugarError
  ErrMultipleNames :: Loc -> TypeName -> DesugarError
  ErrMultipleXtor  :: Loc -> XtorName -> DesugarError
  ErrMultipleAnnot :: Loc -> Variable -> PolTy -> PolTy -> DesugarError
  ErrOther         :: Loc -> String -> DesugarError

instance Error DesugarError where 
  getMessage (ErrVariable _ var) = "Definition for variable " <> show var <> "could not be found"
  getMessage (ErrMultipleNames _ tyn) = show tyn <> " was defined multiple times"
  getMessage (ErrMultipleAnnot _ var ty1 ty2) = "Multiple incompatible annotations for variable" <> show var <> ": " <> show ty1 <> " and " <> show ty2
  getMessage (ErrMultipleXtor _ xtn) = show xtn <> " was defined multiple times "
  getMessage (ErrOther _ str) = str

  getLocation (ErrVariable loc _) = loc
  getLocation (ErrMultipleNames loc _) = loc 
  getLocation (ErrMultipleXtor loc _) = loc
  getLocation (ErrMultipleAnnot loc _ _ _) = loc 
  getLocation (ErrOther loc _) = loc

  toError = ErrOther 
