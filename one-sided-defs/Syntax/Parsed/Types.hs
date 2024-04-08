module Syntax.Parsed.Types (
  PolTy (..),
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

data PolTy = MkPolTy !Ty !Pol
  deriving (Eq,Ord)
