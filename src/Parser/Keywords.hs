module Parser.Keywords where 

data Keyword = 
  KwModule
  | KwData
  | KwVar
  | Kwmu
  | KwMu
  | KwCase
  | KwLam
  | KwDone

instance Show Keyword where 
  show KwModule = "module"
  show KwData   = "data" 
  show KwVar    = "var"
  show KwMu     = "Mu"
  show Kwmu     = "mu"
  show KwCase   = "case"
  show KwLam    = "lam"
  show KwDone   = "Done"
