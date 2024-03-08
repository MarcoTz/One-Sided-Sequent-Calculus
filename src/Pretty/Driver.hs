module Pretty.Driver where 

import Driver.Definition
import Pretty.Program ()
import Pretty.Environment()

instance Show DriverState where 
  show (MkDriverState _ env modules) = "Current environment: " <> show env  <> "\n" <> "current modules: " <> show modules 
