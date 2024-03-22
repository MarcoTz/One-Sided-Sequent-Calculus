module Pretty.Environment where 

import Environment 
import Pretty.Typed ()

instance Show Environment where 
  show (MkEnv defs) = show defs 
