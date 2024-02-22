module Pretty.Common where 

import Common 

instance Show Pol where 
  show Pos = "+"
  show Neg = "-"

instance Show Kind where 
  show (MkKind p) = show p
  show (MkKindVar kv) = kv
