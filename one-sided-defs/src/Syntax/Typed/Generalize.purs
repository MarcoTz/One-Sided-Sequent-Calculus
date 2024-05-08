module Syntax.Typed.Generalize (
  class GeneralizeTy,
  generalizeTy
) where 

import FreeVars.FreeTypevars (freeTypevars)
import Syntax.Typed.Types (Ty(..))
import Syntax.Typed.Terms (Term,setType,getType)
import Syntax.Typed.Program (VarDecl(..))

import Data.Set (isEmpty,toUnfoldable,union,fromFoldable)

class GeneralizeTy a where 
  generalizeTy :: a -> a 

instance GeneralizeTy Ty where 
  generalizeTy (TyForall args ty) = do
    let frVs = freeTypevars ty 
    let newVs = union frVs (fromFoldable args)
    TyForall (toUnfoldable newVs) ty
  generalizeTy ty = do
    let frVs = freeTypevars ty 
    if isEmpty frVs then ty else TyForall (toUnfoldable frVs) ty

instance GeneralizeTy Term where 
  generalizeTy t = setType t (generalizeTy (getType t))

instance GeneralizeTy VarDecl where 
  generalizeTy (VarDecl var) = 
    let newBody = generalizeTy var.varBody in
    VarDecl var{varBody=newBody}
