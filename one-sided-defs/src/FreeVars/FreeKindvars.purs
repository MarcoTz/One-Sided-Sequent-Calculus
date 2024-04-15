module FreeVars.FreeKindvars where 

import Common (Kind(..), Kindvar(..), freshVarN)
import Syntax.Kinded.Types (Ty(..))
import Syntax.Kinded.Terms (Term(..),Pattern(..),Command(..))

import Prelude ((<$>))
import Data.Set (Set,empty,singleton, union,unions)
import Data.List (List(..))

class FreeKindvars a where 
  freeKindvars :: a -> Set Kindvar 

freshKindvar :: forall a.FreeKindvars a => a -> Kindvar 
freshKindvar a = let frV = freeKindvars a in freshVarN 0 "k" (\x -> Kindvar {unKindvar:x}) frV


instance FreeKindvars Kind where
  freeKindvars (MkKind _) = empty 
  freeKindvars (MkKindVar v) = singleton v


instance FreeKindvars Ty where
  freeKindvars (TyVar _ knd) = freeKindvars knd
  freeKindvars (TyDecl _ args knd) = unions (Cons (freeKindvars knd) (freeKindvars <$> args))
  freeKindvars (TyShift ty knd) = union (freeKindvars ty) (freeKindvars knd)
  freeKindvars (TyCo ty) = freeKindvars ty
  freeKindvars (TyForall _ ty) = freeKindvars ty

instance FreeKindvars Term where
  freeKindvars (Var _ _ ty)        = freeKindvars ty
  freeKindvars (Mu _ _ c ty)       = union (freeKindvars ty) (freeKindvars c)
  freeKindvars (Xtor _ _ args ty)  = unions (Cons (freeKindvars ty) (freeKindvars <$> args))
  freeKindvars (XCase _ pts ty)    = unions (Cons (freeKindvars ty) (freeKindvars <$> pts))
  freeKindvars (ShiftCBV _ t ty)   = union (freeKindvars t) (freeKindvars ty)
  freeKindvars (ShiftCBN _ t ty)   = union (freeKindvars t) (freeKindvars ty)

instance FreeKindvars Pattern where 
  freeKindvars (Pattern pt) = freeKindvars pt.ptcmd 

instance FreeKindvars Command where 
  freeKindvars (Cut _ t1 _ t2) = union (freeKindvars t1) (freeKindvars t2) 
  freeKindvars (Done _) = empty
  freeKindvars (Err _ _) = empty
  freeKindvars (Print _ t) = freeKindvars t
