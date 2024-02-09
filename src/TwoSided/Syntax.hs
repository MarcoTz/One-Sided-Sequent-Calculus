module TwoSided.Syntax where 


-- Polarity 
data Pol = Pos | Neg


--Variables 
type Variable = String
type Covariable = String

-- Xtors 
type Destructor = String 
type Constructor = String

-- commands 
data Command = Cut !Producer !Pol !Consumer

-- producers 

data Pattern a = MkPattern { xtor :: !a, patvars :: ![Variable], patcovars :: ![Covariable], patcmd :: !Command}

data Producer = 
    Var !Variable 
  | Mu !Covariable !Command
  | Constr !Constructor ![Producer] ![Consumer]
  | Cocase ![Pattern Destructor] 
  | ShiftCBN !Producer
  | Lambda !Covariable !Command

data Consumer = 
    Covar !Covariable 
  | MuTilde !Variable !Command
  | Destr !Destructor ![Producer] ![Consumer]
  | Case ![Pattern Constructor]
  | ShiftCBV !Consumer
  | LambdaBar !Variable !Command


-- Values 
isValue :: Producer -> Pol -> Bool
isValue (Var _) Pos                       = True
isValue (Constr _ prodargs consargs) Pos  = 
  all (`isCovalue` Neg) consargs && 
  all (`isValue` Pos) prodargs
isValue (Cocase _) Pos                    = True 
isValue (ShiftCBN _) Pos                  = True
isValue _ Pos                             = False 
isValue (Var _) Neg                       = True
isValue _ Neg                             = True

-- Covalues
isCovalue :: Consumer -> Pol -> Bool
isCovalue (Covar _) Neg                   = True 
isCovalue (Destr _ prodargs consargs) Neg = 
  all (`isValue` Pos) prodargs && 
  all (`isCovalue` Neg) consargs
isCovalue (Case _) Neg                    = True 
isCovalue (ShiftCBV _) Neg                = True
isCovalue _ Neg                           = False
isCovalue _ Pos                           = True
