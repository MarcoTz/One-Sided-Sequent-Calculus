module Parser.Keywords where 

data Keyword = 
  KwData
  | KwWhere 

instance Show Keyword where 
  show KwData = "data" 
  show KwWhere = "where"
