module FreeVars.FreeVariables (
  class FreeVariables,
  freeVars,
  freshVar
  ) where

import Common (Variable(..),freshVarN) 
import Syntax.Kinded.Terms (Term(..),Pattern(..),Command(..))

import Prelude ((<$>))
import Data.List (List,foldr)
import Data.Set (Set,unions,singleton,delete,union,empty)

class FreeVariables a where 
  freeVars :: a -> Set Variable 

instance FreeVariables a => FreeVariables (List a) where 
  freeVars ls = unions (freeVars <$> ls)

freshVar :: forall a.FreeVariables a => a -> Variable
freshVar a = let frV = freeVars a in freshVarN 0 "x" Variable frV

instance FreeVariables Term where 
  freeVars (Var _ v _)          = singleton v
  freeVars (Mu _ v c _)         = delete v (freeVars c)
  freeVars (Xtor _ _ args _)    = unions (freeVars <$> args)
  freeVars (XCase _ pts _)      = unions (freeVars <$> pts)
  freeVars (ShiftCBV _ t _)     = freeVars t 
  freeVars (ShiftCBN _ t _)     = freeVars t 

instance FreeVariables Pattern where 
  freeVars (Pattern pt) = foldr delete (freeVars pt.ptcmd) pt.ptv

instance FreeVariables Command where 
  freeVars (Cut _ t1 _ t2) = union (freeVars t1) (freeVars t2) 
  freeVars (Done _) = empty
  freeVars (Err _ _)  = empty
  freeVars (Print _ t) = freeVars t
