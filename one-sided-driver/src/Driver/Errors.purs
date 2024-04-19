module Driver.Errors ( DriverError (..) ) where 

import Loc (Loc,defaultLoc)
import Common (Modulename)
import Errors (class Error,getMessage,getLocation)

import Prelude ((<>),show)

data DriverError =
  ErrTypeInference Loc 
  | ErrWithWhere   DriverError String
  | ErrNotFound    Modulename
  | ErrOther       Loc String 

instance Error DriverError where 
  getMessage (ErrTypeInference _) = "Type Inference is not implemented yet"
  getMessage (ErrWithWhere err str) = getMessage err <> " during " <> str
  getMessage (ErrNotFound mn) = "Could not find " <> show mn <> " in Environment"
  getMessage (ErrOther _ str) = str

  getLocation (ErrTypeInference loc) = loc
  getLocation (ErrOther loc _) = loc
  getLocation (ErrNotFound _) = defaultLoc
  getLocation (ErrWithWhere err _) = getLocation err
  
  toError = ErrOther
