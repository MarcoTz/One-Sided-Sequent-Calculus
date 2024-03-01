module Pretty.Environment where 

import Environment 
import Pretty.Program ()

instance Show Environment where 
  show (MkEnv defs) = show defs 
