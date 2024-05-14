module Syntax.Kinded.Substitution (
  class SubstituteKindvars,
  substKindvars,
  class SubstituteVariables,
  substVars,
  substituteVariable
) where 

import Syntax.Kinded.Types (Ty(..))
import Syntax.Kinded.Terms (Term(..),Command(..),Pattern(..))
import Common (Kindvar, EvaluationOrder, Kind(..), Variable)

import Prelude ((<$>))
import Data.Map (Map, lookup, delete, fromFoldable)
import Data.Tuple (Tuple(..))
import Data.List (foldr)
import Data.Maybe (fromMaybe, maybe)

--------------------------------
-- Kind Variable Substitution --
--------------------------------
class SubstituteKindvars a where 
  substKindvars :: Map Kindvar EvaluationOrder -> a -> a 

instance SubstituteKindvars Kind where 
  substKindvars _ p@(MkKind _) = p
  substKindvars varmap k@(MkKindVar v) = maybe k MkKind (lookup v varmap)

instance SubstituteKindvars Ty where 
  substKindvars varmap (TyVar v knd) = TyVar v (substKindvars varmap knd) 
  substKindvars varmap (TyDecl tyn args knd) = TyDecl tyn (substKindvars varmap <$> args) (substKindvars varmap knd)
  substKindvars varmap (TyShift ty knd) = TyShift (substKindvars varmap ty) (substKindvars varmap knd)
  substKindvars varmap (TyCo ty) = TyCo (substKindvars varmap ty) 
  substKindvars varmap (TyForall args ty) = TyForall args (substKindvars varmap ty)

instance SubstituteKindvars Command where 
  substKindvars varmap (Cut loc t pol u) = Cut loc (substKindvars varmap t) pol (substKindvars varmap u)
  substKindvars _ (Done loc) = Done loc
  substKindvars _ (Err loc msg) = Err loc msg
  substKindvars varmap (Print loc t) = Print loc (substKindvars varmap t)

instance SubstituteKindvars Term where 
  substKindvars varmap (Var loc pc v ty) = Var loc pc v (substKindvars varmap ty)
  substKindvars varmap (Mu loc pc v c ty) = Mu loc pc v (substKindvars varmap c) (substKindvars varmap ty)
  substKindvars varmap (Xtor loc pc nm args ty) = Xtor loc pc nm (substKindvars varmap <$> args) (substKindvars varmap ty)
  substKindvars varmap (XCase loc pc pts ty) = XCase loc pc (substKindvars varmap <$> pts) (substKindvars varmap ty)
  substKindvars varmap (ShiftCBV loc pc t ty) = ShiftCBV loc pc (substKindvars varmap t) (substKindvars varmap ty)
  substKindvars varmap (ShiftCBN loc pc t ty) = ShiftCBN loc pc (substKindvars varmap t) (substKindvars varmap ty)

instance SubstituteKindvars Pattern where 
  substKindvars varmap (Pattern pt) = Pattern (pt {ptcmd=(substKindvars varmap pt.ptcmd)})

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
