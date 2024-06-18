module Syntax.Typed.Substitution ( 
  class SubstituteTypevars,
  substTyvars
) where 

import Syntax.Typed.Types  (Ty(..))
import Syntax.Typed.Program (XtorSig (..),VarDecl(..))
import Syntax.Typed.Terms (Term (..), Pattern(..), Command(..))
import FreeVars.FreeTypevars (freeTypevars)
import Common (Typevar,PrdCns)

import Prelude ((<$>),($))
import Data.Map (Map, lookup, delete)
import Data.List (foldr)
import Data.Set (isEmpty)
import Data.Maybe (Maybe(..))
import Data.Bifunctor (rmap)
import Data.Tuple (Tuple(..))

--------------------------------
-- Type Variable Substitution --
--------------------------------
class SubstituteTypevars a where 
  substTyvars :: Map Typevar Ty -> a -> a 

instance SubstituteTypevars VarDecl where 
  substTyvars varmap (VarDecl v) = VarDecl v{varBody=substTyvars varmap v.varBody}
instance SubstituteTypevars XtorSig where 
  substTyvars varmap (XtorSig sig) = XtorSig (sig {sigArgs=(rmap (substTyvars varmap) <$> sig.sigArgs)})

instance SubstituteTypevars Ty where 
  substTyvars varmap ty@(TyVar v) = case lookup v varmap of 
    Nothing -> ty 
    Just ty' -> if isEmpty $ freeTypevars ty' then ty' else substTyvars varmap ty'
  substTyvars varmap (TyDecl tyn args) = TyDecl tyn (substTyvars varmap <$> args)
  substTyvars varmap (TyShift ty) = TyShift (substTyvars varmap ty)
  substTyvars varmap (TyCo ty) = TyCo (substTyvars varmap ty) 
  substTyvars varmap (TyForall vars ty) = let newmap = foldr delete varmap vars in TyForall vars (substTyvars newmap ty)

instance SubstituteTypevars (Tuple PrdCns Ty) where 
  substTyvars varmap (Tuple pc ty) = Tuple pc (substTyvars varmap ty)

instance SubstituteTypevars Term where 
  substTyvars varmap (Var loc v ty) = Var loc v (substTyvars varmap ty)
  substTyvars varmap (Mu loc v c ty) = Mu loc v (substTyvars varmap c) (substTyvars varmap ty)
  substTyvars varmap (Xtor loc nm args ty) = Xtor loc nm (substTyvars varmap <$> args) (substTyvars varmap ty)
  substTyvars varmap (XCase loc pts ty) = XCase loc (substTyvars varmap <$> pts) (substTyvars varmap ty)
  substTyvars varmap (ShiftCBV loc t ty) = ShiftCBV loc (substTyvars varmap t) (substTyvars varmap ty)
  substTyvars varmap (ShiftCBN loc t ty) = ShiftCBN loc (substTyvars varmap t) (substTyvars varmap ty)

instance SubstituteTypevars Pattern where 
  substTyvars varmap (Pattern pt) = Pattern (pt {ptcmd=(substTyvars varmap pt.ptcmd)})

instance SubstituteTypevars Command where 
  substTyvars varmap (Cut loc t pol u) = Cut loc (substTyvars varmap t) pol (substTyvars varmap u) 
  substTyvars _ (Done loc) = Done loc
  substTyvars _ (Err loc err) = Err loc err
  substTyvars varmap (Print loc t) = Print loc (substTyvars varmap t)
