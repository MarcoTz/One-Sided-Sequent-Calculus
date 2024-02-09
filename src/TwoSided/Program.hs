module TwoSided.Program where 

import TwoSided.Syntax
import TwoSided.Types

-- Declarations 
data TypeDeclaration (a::Xtor) = MkTyDecl {declName :: !TypeName, declTyArgs :: ![(TypeVar, Pol)], declPol :: !Pol, declSig :: ![XtorSig a]}

data Declaration = 
  DataDecl !(TypeDeclaration Ctor)
  | CodataDecl !(TypeDeclaration Dtor)
  | MkVal {valVar :: !Variable, valTy :: !Type, valProd :: !Producer}
  | MkCoval {covalCovar :: !Covariable, covalTy :: !Type, covalCons :: !Consumer}
  | MkRec {recVar :: !Variable, recTy :: !Type, recProd :: !Producer}
  | MkCorec {corecVar :: !Covariable, corecTy :: !Type, corecCons :: !Consumer}
  | Epsilon
  | DeclCons !Declaration !Declaration


---- Signature 
-- Signature 
type XtorName = String
data Xtor = Ctor | Dtor
data XtorRep xt where 
  CtorRep :: XtorRep Ctor 
  DtorRep :: XtorRep Dtor

data XtorSig (a::Xtor) = MkXtorSig {sigName :: !XtorName, sigXtor :: !(XtorRep a), sigProdArgs :: ![Type], sigConsArgs :: ![Type]} 
