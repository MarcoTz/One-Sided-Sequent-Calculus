module Driver.Errors ( DriverError (..) ) where 

import Loc 
import Errors 

data DriverError where
  ErrTypeInference :: Loc -> DriverError
  ErrOther :: Loc -> String -> DriverError

instance Error DriverError where 
  getMessage (ErrTypeInference _) = "Type Inference is not implemented yet"
  getMessage (ErrOther _ str) = str

  getLocation (ErrTypeInference loc) = loc
  getLocation (ErrOther loc _) = loc
  
  toError = ErrOther
