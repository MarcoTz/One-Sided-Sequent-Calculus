module Driver.Errors ( DriverError (..) ) where 

import Loc 
import Errors 

data DriverError where
  ErrTypeInference :: Loc -> DriverError
  ErrWithWhere :: DriverError -> String -> DriverError
  ErrOther :: Loc -> String -> DriverError

instance Error DriverError where 
  getMessage (ErrTypeInference _) = "Type Inference is not implemented yet"
  getMessage (ErrWithWhere err str) = getMessage err <> " during " <> str
  getMessage (ErrOther _ str) = str

  getLocation (ErrTypeInference loc) = loc
  getLocation (ErrOther loc _) = loc
  getLocation (ErrWithWhere err _) = getLocation err
  
  toError = ErrOther
