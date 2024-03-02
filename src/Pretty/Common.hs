module Pretty.Common where 

import Common 

instance Show Pol where 
  show Pos = "+"
  show Neg = "-"

instance Show KindVar where 
  show (MkKVar kv) = kv

instance Show Variable where 
  show (MkVar v) = v

instance Show XtorName where 
  show (MkXtorName xtn) = xtn 

instance Show TypeVar where 
  show (MkTypeVar tyv) = tyv

instance Show TypeName where 
  show (MkTypeName tyn) = tyn

instance Show PolVar where 
  show (MkPolVar tyv pol) = show tyv <> ":" <> show pol

instance Show Kind where 
  show (MkKind p) = show p
  show (MkKindVar kv) = show kv
