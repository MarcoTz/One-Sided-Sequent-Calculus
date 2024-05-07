module Syntax.Desugared.Substitution where 

import Common (Variable)
import Syntax.Desugared.Terms (Term(..),Command(..),Pattern(..))

import Prelude ((<$>))
import Data.Map (Map,lookup,delete,fromFoldable)
import Data.Maybe (fromMaybe)
import Data.List (foldr)
import Data.Tuple (Tuple(..))
--------------------------------------------------------------
----------------- Term Variable Substitution -----------------
--------------------------------------------------------------

class SubstituteVariables a where 
  substVars :: Map Variable Term -> a -> a 

instance SubstituteVariables Term where 
  substVars varmap vt@(Var _ v) = fromMaybe vt (lookup v varmap)
  substVars varmap (Mu loc v c) = Mu loc v (substVars (delete v varmap) c)
  substVars varmap (Xtor loc nm args) = Xtor loc nm (substVars varmap <$> args )
  substVars varmap (XCase loc pts) = XCase loc (substVars varmap <$> pts)
  substVars varmap (ShiftCBV loc t) = ShiftCBV loc (substVars varmap t) 
  substVars varmap (ShiftCBN loc t) = ShiftCBN loc (substVars varmap t) 

instance SubstituteVariables Pattern where 
  substVars varmap (Pattern pt) = let newMap = foldr delete varmap  pt.ptv in Pattern (pt {ptcmd=(substVars newMap pt.ptcmd)})

instance SubstituteVariables Command where 
  substVars varmap (Cut loc t eo u) = Cut loc (substVars varmap t) eo (substVars varmap u)
  substVars _ d@(Done _) = d
  substVars _ e@(Err _ _) = e
  substVars varmap (Print loc t) = Print loc (substVars varmap t)
  substVars varmap (CutAnnot loc t ty eo u) = CutAnnot loc (substVars varmap t) ty eo (substVars varmap u)
  substVars varmap (PrintAnnot loc t ty) = PrintAnnot loc (substVars varmap t) ty 

substituteVariable :: forall a.SubstituteVariables a => Variable -> Term -> a -> a
substituteVariable substVar substT a = let varmap = fromFoldable [Tuple substVar substT] in substVars varmap a 
