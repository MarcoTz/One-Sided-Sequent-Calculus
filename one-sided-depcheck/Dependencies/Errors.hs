module Dependencies.Errors ( DepError (..)) where 

import Common 
import Errors 
import Loc
import Pretty.Common ()

data DepError where
  ErrDuplModule      :: Modulename -> DepError
  ErrUndefinedModule :: Modulename -> DepError
  ErrMutualRec       :: Modulename -> DepError
  ErrUndefinedVar    :: Loc -> Variable -> DepError
  ErrGeneric         :: Loc -> String -> DepError

instance Error DepError where 
    getMessage (ErrDuplModule mn) = "Module " <> show mn <> " was defined multiple times"
    getMessage (ErrUndefinedModule mn) = "Module " <> show mn <> " was not defined"
    getMessage (ErrMutualRec mn) = "Mutual Recusrion in module " <> show mn
    getMessage (ErrUndefinedVar _ v) = "Variable " <> show v <> " was not defined"
    getMessage (ErrGeneric _ str) = str

    getLocation (ErrDuplModule _) = defaultLoc
    getLocation (ErrUndefinedModule _) = defaultLoc
    getLocation (ErrMutualRec _) = defaultLoc
    getLocation (ErrUndefinedVar loc _) = loc 
    getLocation (ErrGeneric loc _) = loc 

    toError = ErrGeneric
