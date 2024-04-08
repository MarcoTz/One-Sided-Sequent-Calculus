module Syntax.Desugared.Types (
  PolTy (..),
  Ty (..)
) where 

import Common 

data Ty where 
  TyVar :: Typevar -> Ty 
  TyDecl :: Typename -> [Ty] -> Ty
  TyCo :: Ty -> Ty
  TyShift :: Ty -> Ty 
  TyForall :: [Typevar] -> Ty -> Ty
  deriving (Eq)

data PolTy = MkPolTy !Ty !Pol
  deriving (Eq)

instance FlipPol PolTy where 
  flipPol (MkPolTy ty pol) = MkPolTy ty (flipPol pol) 
