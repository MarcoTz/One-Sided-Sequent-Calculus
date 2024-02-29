module Pretty.Environment where 

import Environment 
import Pretty.Program ()

instance Show Environment where 
  show (MkEnv decls vars) = show decls <> "\n" <> show vars
