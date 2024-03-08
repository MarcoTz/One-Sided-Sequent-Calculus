module Parser.Keywords where 

data Keyword = 
  KwModule
  | KwData
  | KwVar
  | Kwmu
  | KwMu
  | KwCase
  | KwDone
  | KwForall
  | Kwforall 
  | KwCo
  | Kwco
  | KwImport
  | KwDefine

instance Show Keyword where 
  show KwModule = "module"
  show KwData   = "data" 
  show KwVar    = "var"
  show KwMu     = "Mu"
  show Kwmu     = "mu"
  show KwCase   = "case"
  show KwDone   = "Done"
  show KwForall = "Forall"
  show Kwforall = "forall"
  show KwCo     = "Co"
  show Kwco     = "co"
  show KwImport = "import"
  show KwDefine = "define"

allKws :: [Keyword]
allKws = [KwModule,KwData,KwVar,Kwmu,KwMu,KwCase,KwDone,KwForall,Kwforall,KwCo,Kwco,KwImport,KwDefine]
