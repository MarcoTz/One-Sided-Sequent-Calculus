module Syntax.Desugared.Types (
  PolTy (..),
  Ty (..)
) where 

import Common 

data Ty where 
  TyVar :: TypeVar -> Ty 
  TyDecl :: TypeName -> [Ty] -> Ty
  TyCo :: Ty -> Ty
  TyShift :: Ty -> Ty 
  TyForall :: [TypeVar] -> Ty -> Ty
  deriving (Eq)

data PolTy = MkPolTy !Ty !Pol
  deriving (Eq)

instance FlipPol PolTy where 
  flipPol (MkPolTy ty pol) = MkPolTy ty (flipPol pol) 
