module Syntax.Parsed.Types (
  KindedTy (..),
  Ty (..)
) where 

import Common

data Ty where 
  TyVar    :: Typevar -> Ty
  TyDecl   :: Typename -> [Ty] -> Ty
  TyCo     :: Ty -> Ty
  TyShift  :: Ty -> Ty 
  TyForall :: [Typevar] -> Ty -> Ty
  deriving (Eq,Ord)

data KindedTy = KindedTy { kindedTy :: !Ty, kindedKind :: !Kind }
  deriving (Eq,Ord)
