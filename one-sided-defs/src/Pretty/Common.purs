module Pretty.Common () where 

import Common 

instance Show EvaluationOrder where 
  show CBV = "CBV"
  show CBN = "CBN"

instance Show DeclTy where 
  show Data = "data"
  show Codata = "codata"

instance Show Variance where 
  show Covariant = "+"
  show Contravariant = "-"

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

instance Show VariantVar where 
  show (VariantVar tyv var) = show tyv <> ":" <> show var

instance Show Kind where 
  show (MkKind p) = show p
  show (MkKindVar kv) = show kv
