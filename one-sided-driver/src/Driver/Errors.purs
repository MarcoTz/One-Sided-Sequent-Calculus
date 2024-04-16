module Driver.Errors ( DriverError (..) ) where 

import Loc (Loc)
import Errors (class Error,getMessage,getLocation)

import Prelude ((<>))

data DriverError =
  ErrTypeInference Loc 
  | ErrWithWhere   DriverError String
  | ErrOther       Loc String 

instance Error DriverError where 
  getMessage (ErrTypeInference _) = "Type Inference is not implemented yet"
  getMessage (ErrWithWhere err str) = getMessage err <> " during " <> str
  getMessage (ErrOther _ str) = str

  getLocation (ErrTypeInference loc) = loc
  getLocation (ErrOther loc _) = loc
  getLocation (ErrWithWhere err _) = getLocation err
  
  toError = ErrOther
