module GenerateConstraints.Types where 

import Loc
import Common
import Errors
import Environment
import GenerateConstraints.Definition
import Syntax.Desugared.Types qualified as D
import Syntax.Typed.Types     qualified as T
import Syntax.Typed.Program   qualified as T 

import Control.Monad

genConstraintsTy :: Loc -> D.Ty -> Pol -> GenM T.Ty
genConstraintsTy _ (D.TyVar v) pol = return $ T.TyVar v pol
genConstraintsTy loc (D.TyDecl tyn args) pol = do 
  decl <- lookupDecl loc tyn 
  let argPols = polvarPol <$> T.declArgs decl
  let argPols' = if pol/=T.declPol decl then flipPol <$> argPols else argPols
  argsZipped <- zipWithError args argPols' (ErrTyArity loc tyn)
  args' <- forM argsZipped (uncurry (genConstraintsTy loc))
  return $ T.TyDecl tyn args' pol
genConstraintsTy loc (D.TyCo ty) pol = do 
  ty' <- genConstraintsTy loc ty (flipPol pol)
  return $ T.TyCo ty'
genConstraintsTy loc (D.TyShift ty) pol = do
  ty' <- genConstraintsTy loc ty (flipPol pol)
  return $ T.TyShift ty' pol
genConstraintsTy loc (D.TyForall args ty) pol = T.TyForall args <$> genConstraintsTy loc ty pol

genConstraintsPolTy :: Loc -> D.PolTy -> GenM T.Ty
genConstraintsPolTy loc (D.MkPolTy ty pol) = genConstraintsTy loc ty pol
