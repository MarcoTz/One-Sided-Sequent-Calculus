module Driver.Errors ( DriverError (..) ) where 

import Loc (Loc,defaultLoc)
import Common (Modulename,Variable)
import Errors (class Error,getMessage,getLocation)

import Prelude ((<>),(<$>),show)
import Data.List (List,intercalate)

data DriverError =
  ErrTypeInference Loc 
  | ErrWithWhere   DriverError Modulename String
  | ErrNotFound    Modulename
  | ErrNotStdLib   (List Modulename)
  | ErrAnnotMismatch Loc Variable
  | ErrOther       Loc String 

instance Error DriverError where 
  getMessage (ErrTypeInference _) = "Type Inference is not implemented yet"
  getMessage (ErrWithWhere err mn str) = getMessage err <> " in module " <> show mn <> " during " <> str
  getMessage (ErrNotFound mn) = "Could not find " <> show mn <> " in Environment"
  getMessage (ErrNotStdLib mns) = "Modules " <> intercalate ", " (show <$> mns) <> " are not in standard library"
  getMessage (ErrAnnotMismatch _ var) = "Type annotation for variable " <> show var <> " does not match inferred type"
  getMessage (ErrOther _ str) = str

  getLocation (ErrTypeInference loc) = loc
  getLocation (ErrOther loc _) = loc
  getLocation (ErrNotFound _) = defaultLoc
  getLocation (ErrNotStdLib _) = defaultLoc
  getLocation (ErrWithWhere err _ _) = getLocation err
  getLocation (ErrAnnotMismatch loc _) =loc
  
  toError = ErrOther
