module Syntax.Kinded.Substitution (
  class SubstituteVariables,
  substVars,
  substituteVariable,
  class SubstituteTypevariables,
  substTyvars
) where 

import Syntax.Kinded.Terms (Term(..),Command(..),Pattern(..))
import Syntax.Kinded.Types (Ty(..))
import Common (Variable,Typevar)

import Prelude ((<$>))
import Data.Map (Map, lookup, delete, fromFoldable)
import Data.Tuple (Tuple(..))
import Data.List (foldr)
import Data.Maybe (fromMaybe)

--------------------------------------------------------------
----------------- Term Variable Substitution -----------------
--------------------------------------------------------------

class SubstituteVariables a where 
  substVars :: Map Variable Term -> a -> a 

instance SubstituteVariables Term where 
  substVars varmap vt@(Var _ _ v _) = fromMaybe vt (lookup v varmap)
  substVars varmap (Mu loc pc v c ty) = Mu loc pc v (substVars (delete v varmap) c) ty
  substVars varmap (Xtor loc pc nm args ty) = Xtor loc pc nm (substVars varmap <$> args ) ty
  substVars varmap (XCase loc pc pts ty) = XCase loc pc (substVars varmap <$> pts) ty
  substVars varmap (ShiftCBV loc pc t ty) = ShiftCBV loc pc (substVars varmap t) ty 
  substVars varmap (ShiftCBN loc pc t ty) = ShiftCBN loc pc (substVars varmap t) ty 

instance SubstituteVariables Pattern where 
  substVars varmap (Pattern pt) = let newMap = foldr delete varmap  pt.ptv in Pattern (pt {ptcmd=(substVars newMap pt.ptcmd)})

instance SubstituteVariables Command where 
  substVars varmap (Cut loc t eo u) = Cut loc (substVars varmap t) eo (substVars varmap u)
  substVars _ d@(Done _) = d
  substVars _ e@(Err _ _) = e
  substVars varmap (Print loc t) = Print loc (substVars varmap t)

substituteVariable :: forall a.SubstituteVariables a => Variable -> Term -> a -> a
substituteVariable substVar substT a = let varmap = fromFoldable [Tuple substVar substT] in substVars varmap a 

class SubstituteTypevariables a where 
  substTyvars :: Map Typevar Ty -> a -> a 
