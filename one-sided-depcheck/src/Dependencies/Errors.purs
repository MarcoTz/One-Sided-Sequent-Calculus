module Dependencies.Errors ( DepError (..)) where 

import Loc (Loc, defaultLoc)
import Errors (class Error)
import Common (Modulename,Variable)

import Prelude ((<>), show)

data DepError =
  ErrDuplModule        Modulename
  | ErrUndefinedModule Modulename
  | ErrMutualRec       Modulename
  | ErrUndefinedVar    Loc Variable
  | ErrOther           Loc String

instance Error DepError where 
    getMessage (ErrDuplModule mn) = "Module " <> show mn <> " was defined multiple times"
    getMessage (ErrUndefinedModule mn) = "Module " <> show mn <> " was not defined"
    getMessage (ErrMutualRec mn) = "Mutual Recusrion in module " <> show mn
    getMessage (ErrUndefinedVar _ v) = "Variable " <> show v <> " was not defined"
    getMessage (ErrOther _ str) = str

    getLocation (ErrDuplModule _) = defaultLoc
    getLocation (ErrUndefinedModule _) = defaultLoc
    getLocation (ErrMutualRec _) = defaultLoc
    getLocation (ErrUndefinedVar loc _) = loc 
    getLocation (ErrOther loc _) = loc 

    toError = ErrOther
