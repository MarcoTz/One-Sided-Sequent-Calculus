module Kinding.Types ( 
  checkKindType
)where 

import Loc (defaultLoc)
import Common (EvaluationOrder(..), VariantVar(..), Variance(..),shiftEvalOrder)
import Kinding.Definition (KindM)
import Kinding.Errors (KindError(..))
import Syntax.Typed.Types (Ty(..))          as T
import Syntax.Kinded.Types (Ty(..))         as K
import Syntax.Kinded.Program (DataDecl(..)) as K
import Environment (lookupDecl)
import Errors (zipWithErrorM)

import Prelude (($),(<$>),(==),pure,bind)
import Data.Traversable (for)
import Data.Tuple (Tuple(..))


checkKindType :: T.Ty -> EvaluationOrder -> KindM K.Ty 
checkKindType (T.TyVar v) eo = pure $ K.TyVar v eo
checkKindType (T.TyDecl nm tyArgs) eo = do
  (K.DataDecl decl) <- lookupDecl defaultLoc nm
  let getArgEo (VariantVar var) = if var.variantVariance == Covariant then pure eo else pure $ shiftEvalOrder eo
  argKinds <- for decl.declArgs getArgEo
  argsZipped <- zipWithErrorM tyArgs argKinds (ErrTyArity defaultLoc nm)
  tyArgs' <- for argsZipped (\(Tuple var eo') -> checkKindType var eo')
  pure $ K.TyDecl nm tyArgs'  CBV
checkKindType (T.TyShift ty) eo = do
  ty' <- checkKindType ty (shiftEvalOrder eo)
  pure $ K.TyShift ty' eo
checkKindType (T.TyCo ty) eo = do
  ty' <- checkKindType ty eo
  pure $ K.TyCo ty' 
checkKindType (T.TyForall args ty) eo = K.TyForall args <$> checkKindType ty eo

