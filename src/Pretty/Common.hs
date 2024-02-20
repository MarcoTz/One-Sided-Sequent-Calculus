module Pretty.Common where 

import Common 

instance Show Pol where 
  show Pos = "+"
  show Neg = "-"

