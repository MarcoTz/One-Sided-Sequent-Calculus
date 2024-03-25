module TypeCheck.Types (
  checkPolTy
) 
where 

import TypeCheck.Definition
import Syntax.Desugared.Types qualified as D
import Syntax.Typed.Types     qualified as T
import Syntax.Typed.Program   qualified as T
import Errors
import Common
import Loc
import Environment

import Control.Monad
import Control.Monad.Except

checkType :: Loc -> D.Ty -> Pol -> CheckM T.Ty
checkType loc (D.TyVar v) pol = do
  tyVars <- getCheckerTyVars 
  if v `elem` tyVars then return $ T.TyVar v pol else throwError (ErrFreeTyVar loc v)

checkType loc (D.TyDecl tyn args) pol = do 
   T.MkData _ _ argVars _  _ <- lookupDecl loc tyn
   polPairs <- zipWithError args (getKind <$> argVars) (ErrTypeArity loc tyn) 
   args' <- forM polPairs (uncurry (checkType loc))
   return $ T.TyDecl tyn args' pol 

checkType loc (D.TyCo ty) pol = T.TyCo <$> checkType loc ty (flipPol pol)
checkType loc (D.TyShift ty) pol = (`T.TyShift` pol) <$> checkType loc ty Pos
checkType loc (D.TyForall args ty) pol = do
  forM_ args addCheckerTyVar 
  T.TyForall args <$> checkType loc ty pol

checkPolTy :: Loc -> D.PolTy -> CheckM T.Ty
checkPolTy _ (D.MkPolTy (D.TyVar v) pol) = return $ T.TyVar v pol
checkPolTy loc (D.MkPolTy (D.TyDecl tyn tyargs) pol) = do 
  T.MkData _ _ tyargs'  _ _ <- lookupDecl loc tyn
  tyArgsZipped <- zipWithError tyargs (getKind <$> tyargs') (ErrTypeArity loc tyn)
  args' <- forM tyArgsZipped (uncurry (checkType loc))
  return $ T.TyDecl tyn args' pol
checkPolTy loc (D.MkPolTy (D.TyCo ty) pol) = T.TyCo <$> checkPolTy loc (D.MkPolTy ty (flipPol pol))
checkPolTy loc (D.MkPolTy (D.TyForall args ty) pol) = do
  forM_ args addCheckerTyVar
  T.TyForall args <$> checkPolTy loc (D.MkPolTy ty pol)
checkPolTy loc (D.MkPolTy (D.TyShift ty) pol) = (`T.TyShift` pol) <$> checkType loc ty Pos
