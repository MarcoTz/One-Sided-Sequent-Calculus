module Syntax.Parsed.Types (
  PolTy (..),
  Ty (..)
) where 

import Common

data Ty where 
  TyVar    :: TypeVar -> Ty
  TyDecl   :: TypeName -> [Ty] -> Ty
  TyCo     :: Ty -> Ty
  TyShift  :: Ty -> Ty 
  TyForall :: [TypeVar] -> Ty -> Ty
  deriving (Eq,Ord)

data PolTy = MkPolTy !Ty !Pol
  deriving (Eq,Ord)
