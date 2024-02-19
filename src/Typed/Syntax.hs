module Typed.Syntax where 

import Untyped.Syntax qualified as S
import Typed.Types 

type XtorName = String

data Command = Cut !Term !S.Pol !Term

data Term = 
  Var !S.Variable !Ty
  | Mu !S.Variable !Command !Ty
  | Xtor !XtorName ![Term] !Ty
  | XCase ![S.Pattern] !Ty
  | Shift !Term !Ty
  | Lam !S.Variable !Command !Ty
