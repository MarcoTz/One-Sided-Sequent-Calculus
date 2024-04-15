module Kinding.Types ( 
  kindType
)where 

import Common (Kind(..),EvaluationOrder(..))
import Kinding.Definition (KindM)
import Syntax.Typed.Types (Ty(..)) as T
import Syntax.Kinded.Types (Ty(..)) as K

import Prelude (($),(<$>),pure,bind)
import Data.Traversable (for)

defaultKind :: Kind 
defaultKind = MkKind CBV

kindType :: T.Ty -> KindM K.Ty
kindType (T.TyVar v) = pure $ K.TyVar v defaultKind 
kindType (T.TyDecl nm tyArgs) = do
  tyArgs' <- for tyArgs kindType
  pure $ K.TyDecl nm tyArgs' defaultKind
kindType (T.TyShift ty) = (\x -> K.TyShift x defaultKind) <$> kindType ty
kindType (T.TyCo ty) = K.TyCo <$> kindType ty 
kindType (T.TyForall args ty) = K.TyForall args <$> kindType ty
