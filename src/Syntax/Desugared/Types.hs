module Syntax.Desugared.Types where 

import Common 


data Ty = 
  TyVar !TypeVar
  | TyDecl !TypeName ![Ty] 
  | TyCo !Ty
  | TyShift !Ty
  | TyForall ![TypeVar] !Ty
  deriving (Eq)

data PolTy = MkPolTy !Ty !Pol
  deriving (Eq)

instance FlipPol PolTy where 
  flipPol (MkPolTy ty pol) = MkPolTy ty (flipPol pol) 
