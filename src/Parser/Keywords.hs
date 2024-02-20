module Parser.Keywords where 

data Keyword = 
  KwModule
  | KwData
  | KwVal
  | Kwmu
  | KwMu
  | KwCase
  | KwLam
  | KwDone

instance Show Keyword where 
  show KwModule = "module"
  show KwData   = "data" 
  show KwVal    = "val"
  show KwMu     = "Mu"
  show Kwmu     = "mu"
  show KwCase   = "case"
  show KwLam    = "lam"
  show KwDone   = "Done"
