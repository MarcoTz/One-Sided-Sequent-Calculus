module Pretty.Driver where 

import Driver.Definition
import Pretty.Program ()

instance Show DriverState where 
  show (MkDriverState _ env) = show env
