module TypeCheck.Terms (
  checkTerm,
  checkCommand
) where 

import TypeCheck.Definition
import TypeCheck.Types
import Syntax.Typed.Terms         qualified as T
import Syntax.Typed.Types         qualified as T
import Syntax.Typed.Program       qualified as T
import Syntax.Typed.FreeVars      qualified as T 
import Syntax.Typed.Substitution  qualified as T
import Syntax.Desugared.Terms     qualified as D
import Environment
import Errors
import Common
import Loc
import Embed.Definition
import Embed.EmbedTyped () 
import Pretty.Typed ()

import Control.Monad
import Control.Monad.Except
import Data.Map qualified as M

checkTerm :: D.Term -> T.Ty -> CheckM T.Term
checkTerm t ty | containsKindvar ty = throwError (ErrKindVar (getLoc t) (embed ty))
checkTerm t (T.TyForall args ty) = do 
  forM_ args addCheckerTyVar 
  t' <- checkTerm t ty
  case t' of 
    T.Var loc v ty' -> return $ T.Var loc v (T.TyForall args ty')
    T.Xtor loc xtn xtargs ty' -> return $ T.Xtor loc xtn xtargs (T.TyForall args ty')
    T.Mu loc v c ty' -> return $ T.Mu loc v c (T.TyForall args ty')
    T.XCase loc pts ty' -> return $ T.XCase loc pts (T.TyForall args ty')
    T.ShiftPos loc t'' ty' -> return $ T.ShiftPos loc t'' (T.TyForall args ty')
    T.ShiftNeg loc v c ty' -> return $ T.ShiftNeg loc v c (T.TyForall args ty')

checkTerm t (T.TyCo ty) = do 
  t' <- checkTerm t (shiftEvalOrder ty)
  case t' of 
    T.Var loc v ty' -> return $ T.Var loc v (T.TyCo ty')
    T.Xtor loc xtn xtargs ty' -> return $ T.Xtor loc xtn xtargs (T.TyCo ty')
    T.Mu loc v c ty' -> return $ T.Mu loc v c (T.TyCo ty')
    T.XCase loc pts ty' -> return $ T.XCase loc pts (T.TyCo ty')
    (T.ShiftPos loc _ _) -> throwError (ErrTyCoForShift loc t' ty)
    (T.ShiftNeg loc _ _ _) -> throwError (ErrTyCoForShift loc t' ty)

checkTerm vart@(D.Var loc v) ty = do
  vars <- getCheckerVars 
  mdecl <- lookupMVar v
  mrec <- lookupMRec v
  case (M.lookup v vars,mdecl,mrec) of 
    (Nothing,Nothing,Nothing) -> throwError (ErrUndefinedVar loc v)
    (Just (T.TyVar tyv pol),_,_) -> do
      unless (pol == getKind ty) $ throwError (ErrKindNeq loc (T.TyVar tyv pol) ty vart)
      tyVars <- getCheckerTyVars 
      if tyv `elem` tyVars then return $ T.Var loc v ty
      else throwError (ErrUndefinedTyVar loc tyv vart)
    (Just ty',_,_) -> T.Var loc v <$> checkTys ty ty'
    (_,Just (T.MkVar _ _ ty' _),_) ->  T.Var loc v <$> checkTys ty ty'
    (_,_,Just (T.MkRec _ _ ty' _)) ->  T.Var loc v <$> checkTys ty ty'
  where 
    checkTys :: T.Ty -> T.Ty -> CheckM T.Ty
    checkTys ty1 ty2 = do
      let ty1' = T.generalizeTy ty1
      let ty2' = T.generalizeTy ty2
      unless (getKind ty1 == getKind ty2) $ throwError (ErrKindNeq loc ty1 ty2 vart)
      if T.isSubsumed ty1' ty2' || T.isSubsumed ty2' ty1' then return ty2
      else throwError (ErrTypeNeq loc ty2' ty1' vart)

checkTerm (D.Mu loc v c) ty = do
  addCheckerVar v (shiftEvalOrder ty)
  c' <- checkCommand c
  return (T.Mu loc v c' ty)

checkTerm xtt@(D.Xtor loc xtn xtargs) ty@(T.TyDecl tyn tyargs (MkKind eo)) = do 
  T.MkData _ tyn' argVars isco _ <- lookupXtorDecl loc xtn 
  unless (eo == defaultEo isco) $ throwError (ErrWrongEo loc ty isco)
  unless (tyn == tyn') $ throwError (ErrNotTyDecl loc tyn' ty xtt)
  let argEos = MkKind . (`varianceEvalOrder` eo) . variantVariance <$> argVars
  tyargsZipped <- zipWithError (getKind <$> tyargs)  argEos (ErrTypeArity loc tyn)
  unless (all (uncurry (==)) tyargsZipped) $ throwError (ErrArgumentKind loc tyn tyargs)
  T.MkXtorSig _ _ xtargs'  <- lookupXtor loc xtn
  varmap <- M.fromList <$> zipWithError (variantVar <$> argVars) tyargs (ErrTypeArity loc tyn)
  let xtargs'' = T.substTyVars varmap <$> xtargs'
  xtArgsZipped <- zipWithError xtargs xtargs'' (ErrXtorArity loc xtn)
  xtargs''' <- forM xtArgsZipped (uncurry checkTerm)
  return (T.Xtor loc xtn xtargs''' ty)

checkTerm xct@(D.XCase loc pts@(pt1:_)) ty@(T.TyDecl tyn tyargs (MkKind eo)) = do 
  T.MkData _ tyn' argVars isco xtors <- lookupXtorDecl loc (D.ptxt pt1)
  unless (eo /= defaultEo isco) $ throwError (ErrWrongEo loc ty isco) 
  unless (tyn == tyn') $ throwError (ErrNotTyDecl loc tyn' ty xct)
  let argEos = MkKind . (`varianceEvalOrder` eo) . variantVariance <$> argVars
  tyArgsZipped <- zipWithError (getKind <$> tyargs) argEos (ErrTypeArity loc tyn)
  unless (all (uncurry (==)) tyArgsZipped) $ throwError (ErrArgumentKind loc tyn tyargs) 
  let ptxtns = D.ptxt <$> pts
  let declxtns = T.sigName <$> xtors
  unless (all (`elem` ptxtns) declxtns) $ throwError (ErrBadPattern loc ptxtns declxtns xct)
  unless (all (`elem` declxtns) ptxtns) $ throwError (ErrBadPattern loc ptxtns declxtns xct)
  varmap <- M.fromList <$> zipWithError (variantVar <$> argVars) tyargs (ErrTypeArity loc tyn)
  pts' <- forM pts (`checkPattern` varmap)
  return $ T.XCase loc pts' ty 
  where 
    checkPattern :: D.Pattern -> M.Map Typevar T.Ty -> CheckM T.Pattern 
    checkPattern (D.MkPattern xtn vars c) varmap = do
      T.MkXtorSig _ _ xtargs <- lookupXtor loc xtn
      let xtargs' = T.substTyVars varmap <$> xtargs
      argsZipped <- zipWithError vars xtargs' (ErrXtorArity loc xtn)
      currVars <- getCheckerVars 
      let newVars = foldr (\(v,ty') m -> M.insert v ty' m)  currVars argsZipped 
      c' <- withCheckerVars newVars (checkCommand c)
      return $ T.MkPattern xtn vars c'

checkTerm shiftt@(D.ShiftPos loc t) shiftty@(T.TyShift ty (MkKind CBV)) = do 
  t' <- checkTerm t ty 
  unless (getKind t' == MkKind CBV) $ throwError (ErrKindNeq loc (T.getType t') shiftty shiftt)
  return $ T.ShiftPos loc t' (T.TyShift (T.getType t') (MkKind CBV))

checkTerm shiftt@(D.ShiftNeg loc v c) shiftty@(T.TyShift ty (MkKind CBN)) = do 
  unless (getKind ty == MkKind CBV) $ throwError (ErrKindNeq loc ty shiftty shiftt)
  addCheckerVar v ty
  c' <- checkCommand c
  return $ T.ShiftNeg loc v c' ty
  
checkTerm t ty = throwError (ErrBadType (getLoc t) t ty)

checkCommand :: D.Command -> CheckM T.Command
checkCommand c@(D.Cut loc (D.Var loc1 v1) pol (D.Var loc2 v2)) = do
  mty1 <- getMTypeVar v1 
  mty2 <- getMTypeVar v2 
  case (mty1,mty2) of
    (Nothing,Nothing) -> throwError (ErrUnclearType loc c)
    (Just ty1,Nothing) -> return $ T.Cut loc (T.Var loc1 v1 ty1) pol (T.Var loc2 v2 (shiftEvalOrder ty1))
    (Nothing,Just ty2) -> return $ T.Cut loc (T.Var loc1 v1 (shiftEvalOrder ty2)) pol (T.Var loc2 v2 ty2)
    (Just ty1,Just ty2) ->  
      if shiftEvalOrder ty1 == ty2 then 
        return $ T.Cut loc (T.Var loc1 v1 ty1) pol (T.Var loc2 v2 ty2) else 
        throwError (ErrUnclearType loc c)
checkCommand c@(D.Cut loc (D.Var loc1 v1) pol u) = do
  mty <- getMTypeVar v1 
  case mty of 
    Nothing -> throwError (ErrUnclearType loc c)
    Just ty -> do 
      u' <- checkTerm u (shiftEvalOrder ty)
      return $ T.Cut loc (T.Var loc1 v1 ty) pol u'
checkCommand c@(D.Cut loc t pol (D.Var loc2 v2)) = do
  mty <- getMTypeVar v2 
  case mty of 
    Nothing -> throwError (ErrUnclearType loc c)
    Just ty -> do 
      t' <- checkTerm t (shiftEvalOrder ty)
      return $ T.Cut loc t' pol (T.Var loc2 v2 ty)
checkCommand c@D.Cut{} = throwError (ErrUnclearType (getLoc c) c)
checkCommand c@(D.CutAnnot loc t ty pol u) = do
  ty' <- checkKindedTy loc ty 
  ty'' <- checkKindedTy loc (shiftEvalOrder ty)
  t' <- checkTerm t ty' 
  u' <- checkTerm u  ty'' 
  let pol1 = getKind t' 
  let pol2 = getKind u' 
  when (pol1 == pol2) $ throwError (ErrCutKind loc (T.getType t') (T.getType u') c)
  return $ T.Cut loc t' pol u'
checkCommand (D.Done loc) = return (T.Done loc)
checkCommand (D.Err loc err) = return $ T.Err loc err
checkCommand c@(D.Print loc (D.Var loc1 v)) = do
  mty <- getMTypeVar v 
  case mty of 
    Nothing -> throwError (ErrUnclearType loc c) 
    Just ty -> return $ T.Print loc (T.Var loc1 v ty) 
checkCommand c@D.Print{} = throwError (ErrUnclearType (getLoc c) c)
checkCommand (D.PrintAnnot loc t ty) = do
  ty' <- checkKindedTy loc ty
  t' <- checkTerm t ty'
  return $ T.Print loc t' 
