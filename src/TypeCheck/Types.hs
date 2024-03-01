module TypeCheck.Types where 

import TypeCheck.Definition
import Syntax.Desugared.Types qualified as D
import Syntax.Typed.Types     qualified as T
import Common

checkType :: D.Ty -> Pol -> CheckM T.Ty
checkType _ _ = error "not implemented (checkType)"
