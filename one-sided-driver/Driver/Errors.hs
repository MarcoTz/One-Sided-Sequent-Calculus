module Driver.Errors ( DriverError (..) ) where 

import Loc 
import Errors 

data DriverError = 
  ErrTypeInference
  | ErrOther !String !Loc

instance Error DriverError where 
  getMessage ErrTypeInference = "Type Inference is not implemented yet"
  getMessage (ErrOther str _) = str
  getLoc _ = defaultLoc
  toError = ErrOther
