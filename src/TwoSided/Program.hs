module TwoSided.Program where 

import TwoSided.Syntax
import TwoSided.Types

-- Declarations 
data Declaration = 
    MkDeclaration { declName :: !TypeName, declTypeArgs :: ![(TypeVar, Pol)], declPol :: !Pol, declSig :: !Signature}
  | MkCodeclaration { codeclName :: !TypeName, codeclTypeArgs :: ![(TypeVar, Pol)], codeclPol :: !Pol, codeclInterface :: !Interface}
  | MkVal {valVar :: !Variable, valTy :: !Type, valProd :: !Producer}
  | MkCoval {covalCovar :: !Covariable, covalTy :: !Type, covalCons :: !Consumer}
  | MkRec {recVar :: !Variable, recTy :: !Type, recProd :: !Producer}
  | MkCorec {corecVar :: !Covariable, corecTy :: !Type, corecCons :: !Consumer}
  | Epsilon
  | DeclCons !Declaration !Declaration

---- Signature 
data Signature = MkSig {sigCons :: !Constructor, sigProdArgs :: ![Type], sigConsArgs :: ![Type]}

-- Interface 
data Interface = MkInter {interDest :: !Destructor, interProdArgs :: ![Type], interConsArgs :: ![Type]}

