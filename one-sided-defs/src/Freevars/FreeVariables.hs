module FreeVars.FreeVariables(
  class FreeVariables,
  freeVars,
  freshVar
)


class FreeVariables a where 
  freeVars :: a -> Set Variable 

instance FreeVariables a => FreeVariables (List a) where 
  freeVars ls = unions (freeVars <$> ls)

freshVar :: forall a.FreeVariables a => a -> Variable
freshVar a = let frV = freeVars a in freshVarN 0 "x" (\x -> Variable {unVariable:x}) frV

instance FreeVariables Term where 
  freeVars (Var _ v _)          = S.singleton v
  freeVars (Mu _ v c _)         = S.delete v (freeVars c)
  freeVars (Xtor _ _ args _)    = S.unions (freeVars <$> args)
  freeVars (XCase _ pts _)      = S.unions (freeVars <$> pts)
  freeVars (ShiftCBV _ t _)     = freeVars t 
  freeVars (ShiftCBN _ t _)     = freeVars t 

instance FreeVariables Pattern where 
  freeVars (MkPattern _ vars cmd) = foldr S.delete (freeVars cmd) vars

instance FreeVariables Command where 
  freeVars (Cut _ t1 _ t2) = S.union (freeVars t1) (freeVars t2) 
  freeVars Done{} = S.empty
  freeVars Err{}  = S.empty
  freeVars (Print _ t) = freeVars t
