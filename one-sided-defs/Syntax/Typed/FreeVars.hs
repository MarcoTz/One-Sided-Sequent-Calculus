module Syntax.Typed.FreeVars (
  freeVars,
  freshVar
) where 

import Common 
import Syntax.Typed.Terms 

import Data.Set qualified as S 

------------------------------
-- Calculate Free Variables --
------------------------------

class FreeVars a where 
  freeVars :: a -> S.Set Variable

instance FreeVars Term where 
  freeVars (Var _ v _)          = S.singleton v
  freeVars (Mu _ v c _)         = S.delete v (freeVars c)
  freeVars (Xtor _ _ args _)    = S.unions (freeVars <$> args)
  freeVars (XCase _ pts _)      = S.unions (freeVars <$> pts)
  freeVars (ShiftPos _ t _)     = freeVars t 
  freeVars (ShiftNeg _ v cmd _) = S.delete v (freeVars cmd)

instance FreeVars Pattern where 
  freeVars MkPattern{ptxt=_, ptv=vars, ptcmd=st} = foldr S.delete (freeVars st) vars

instance FreeVars Command where 
  freeVars (Cut _ t1 _ t2) = S.union (freeVars t1) (freeVars t2) 
  freeVars Done{} = S.empty
  freeVars Err{}  = S.empty

freshVar :: Int -> S.Set Variable -> Variable 
freshVar n vars = let newV = MkVariable ("x"<> show n) in if newV `elem` vars then freshVar (n+1) vars else newV
