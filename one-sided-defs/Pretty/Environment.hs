module Pretty.Environment () where 

import Environment 
import Pretty.Kinded ()

instance Show Environment where 
  show (MkEnv defs) = show defs 
