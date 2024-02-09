module TwoSided.Syntax where 

-- polarity 
data Pol = Pos | Neg

--variables 
type Variable = String
type Covariable = String

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


-- Declarations 
type TypeName = String
type TypeVar = String
data Declaration = 
    MkDeclaration { declName :: !TypeName, declTypeArgs :: ![(TypeVar, Pol)], declPol :: !Pol, declSig :: !Signature}
  | MkCodeclaration { codeclName :: !TypeName, codeclTypeArgs :: ![(TypeVar, Pol)], codeclPol :: !Pol, codeclInterface :: !Interface}
  | MkVal {valVar :: !Variable, valTy :: !Type, valProd :: !Producer}
  | MkCoval {covalCovar :: !Covariable, covalTy :: !Type, covalCons :: !Consumer}
  | MkRec {recVar :: !Variable, recTy :: !Type, recProd :: !Producer}
  | MkCorec {corecVar :: !Covariable, corecTy :: !Type, corecCons :: !Consumer}
  | Epsilon
  | DeclCons !Declaration !Declaration
--
---- Signature 
type Constructor = String
data Signature = MkSig {sigCons :: !Constructor, sigProdArgs :: ![Type], sigConsArgs :: ![Type]}

-- Interface 
type Destructor = String 
data Interface = MkInter {interDest :: !Destructor, interProdArgs :: ![Type], interConsArgs :: ![Type]}
-- Types 
data Type = TyVar !TypeVar | TyDeclared !TypeName ![Type] | TyDown !Type | TyUp !Type
