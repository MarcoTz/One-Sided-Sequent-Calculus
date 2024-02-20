module Parser.Keywords where 

data Keyword = 
  KwModule
  | KwData
  | KwWhere 

instance Show Keyword where 
  show KwModule = "module"
  show KwData = "data" 
  show KwWhere = "where"
