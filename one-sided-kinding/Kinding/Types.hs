module Kinding.Types ( 
  kindType
)where 

import Kinding.Definition
import Common
import Syntax.Typed.Types qualified as T
import Syntax.Kinded.Types qualified as K

import Control.Monad

defaultKind :: Kind 
defaultKind = MkKind CBV

kindType :: T.Ty -> KindM K.Ty
kindType (T.TyVar v) = return $ K.TyVar v defaultKind 
kindType (T.TyDecl nm tyArgs) = do
  tyArgs' <- forM tyArgs kindType
  return $ K.TyDecl nm tyArgs' defaultKind
kindType (T.TyShift ty) = (`K.TyShift` defaultKind) <$> kindType ty
kindType (T.TyCo ty) = K.TyCo <$> kindType ty 
kindType (T.TyForall args ty) = K.TyForall args <$> kindType ty
