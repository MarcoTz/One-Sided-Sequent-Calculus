module Dependencies.Errors ( DepError (..)) where 

import Common 
import Errors 
import Loc
import Pretty.Common ()

data DepError = 
  ErrDuplModule !Modulename
  | ErrUndefinedModule !Modulename
  | ErrMutualRec !Modulename
  | ErrUndefinedVar !Variable
  | ErrGeneric !String !Loc

instance Error DepError where 
    getMessage (ErrDuplModule mn) = "Module " <> show mn <> " was defined multiple times"
    getMessage (ErrUndefinedModule mn) = "Module " <> show mn <> " was not defined"
    getMessage (ErrMutualRec mn) = "Mutual Recusrion in module " <> show mn
    getMessage (ErrUndefinedVar v) = "Variable " <> show v <> " was not defined"
    getMessage (ErrGeneric str _) = str
    getLoc _ = defaultLoc
    toError = ErrGeneric 
