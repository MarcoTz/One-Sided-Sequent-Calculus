module Pretty.Driver () where 

import Driver.Definition
import Pretty.Typed ()
import Pretty.Environment()

instance Show DriverState where 
  show (MkDriverState _ env ) = "Current environment: " <> show env  
