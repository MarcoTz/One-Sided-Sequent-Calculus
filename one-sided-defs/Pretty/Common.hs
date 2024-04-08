module Pretty.Common () where 

import Common 

instance Show Pol where 
  show Pos = "+"
  show Neg = "-"

instance Show Modulename where 
  show (Modulename nm) = nm 

instance Show Kindvar where 
  show (Kindvar kv) = kv

instance Show Variable where 
  show (Variable v) = v

instance Show Xtorname where 
  show (Xtorname xtn) = xtn 

instance Show Typevar where 
  show (Typevar tyv) = tyv

instance Show Typename where 
  show (Typename tyn) = tyn

instance Show Polvar where 
  show (Polvar tyv pol) = show tyv <> ":" <> show pol

instance Show Kind where 
  show (MkKind p) = show p
  show (MkKindVar kv) = show kv
