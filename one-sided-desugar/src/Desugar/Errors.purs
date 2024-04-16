module Desugar.Errors (DesugarError (..)) where 

import Common (Variable, Typename, Xtorname)
import Loc (Loc)
import Errors (class Error)
import Syntax.Desugared.Types (Ty)

import Prelude ((<>), show)

data DesugarError =
  ErrVariable        Loc Variable 
  | ErrMultipleNames Loc Typename
  | ErrMultipleXtor  Loc Xtorname
  | ErrMultipleAnnot Loc Variable Ty Ty
  | ErrOther         Loc String

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
