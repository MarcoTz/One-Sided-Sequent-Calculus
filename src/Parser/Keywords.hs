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
  | KwForall
  | Kwforall 
  | KwCo
  | Kwco

instance Show Keyword where 
  show KwModule = "module"
  show KwData   = "data" 
  show KwVar    = "var"
  show KwMu     = "Mu"
  show Kwmu     = "mu"
  show KwCase   = "case"
  show KwLam    = "lam"
  show KwDone   = "Done"
  show KwForall = "Forall"
  show Kwforall = "forall"
  show KwCo     = "Co"
  show Kwco     = "co"

allKws :: [Keyword]
allKws = [KwModule,KwData,KwVar,Kwmu,KwMu,KwCase,KwLam,KwDone,KwForall,Kwforall,KwCo,Kwco]

