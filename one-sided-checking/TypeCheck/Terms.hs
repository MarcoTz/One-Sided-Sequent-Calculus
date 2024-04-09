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
import Data.Bifunctor (first)

-----------------------------------------------------------------------------
----------------------------------- Terms -----------------------------------
-----------------------------------------------------------------------------
checkTerm :: D.Term -> T.Ty -> CheckM T.Term
checkTerm t ty = do 
  mt <- tryCheckTerm t ty 
  case mt of 
    Left t' -> return t'
    Right err -> throwError err

tryCheckTerm :: D.Term -> T.Ty -> CheckM (Either T.Term CheckerError)
tryCheckTerm t ty | containsKindvar ty = return $ Right (ErrKindVar (getLoc t) (embed ty))
tryCheckTerm t (T.TyForall args ty) = do 
  forM_ args addCheckerTyVar 
  t' <- tryCheckTerm t ty
  let leftFun ter = T.setType ter (T.TyForall args (T.getType ter))
  return $ first leftFun t'

tryCheckTerm t (T.TyCo ty) = do 
  t' <- tryCheckTerm t (shiftEvalOrder ty)
  let leftFun ter  = T.setType ter (T.TyCo (T.getType ter))
  return $ first leftFun t'

tryCheckTerm vart@(D.Var loc v) ty = do
  vars <- getCheckerVars 
  mdecl <- lookupMVar v
  mrec <- lookupMRec v
  case (M.lookup v vars,mdecl,mrec) of 
    (Nothing,Nothing,Nothing) -> return $ Right (ErrUndefinedVar loc v)
    (Just (T.TyVar tyv knd),_,_) | knd /= getKind ty -> return $ Right (ErrKindNeq loc (T.TyVar tyv knd) ty vart)
    (Just (T.TyVar tyv _),_,_) -> do 
      tyVars <- getCheckerTyVars 
      if tyv `elem` tyVars then return $ Left (T.Var loc v ty)
      else return $ Right (ErrUndefinedTyVar loc tyv vart)
    (Just ty',_,_) -> do 
      ty'' <- checkTys ty ty'
      case ty'' of Left ty''' -> return $ Left (T.Var loc v ty'''); Right err -> return $ Right err
    (_,Just (T.MkVar _ _ ty' _),_) ->  do
      ty'' <- checkTys ty ty'
      case ty'' of Left ty''' -> return $ Left (T.Var loc v ty'''); Right err -> return $ Right err
    (_,_,Just (T.MkRec _ _ ty' _)) ->  do 
      ty'' <- checkTys ty ty'
      case ty'' of Left ty''' -> return $ Left (T.Var loc v ty'''); Right err -> return $ Right err
  where 
    checkTys :: T.Ty -> T.Ty -> CheckM (Either T.Ty CheckerError)
    checkTys ty1 ty2 | getKind ty1 /= getKind ty2 = return $ Right (ErrKindNeq loc ty1 ty2 vart) 
    checkTys ty1 ty2 | T.isSubsumed (T.generalizeTy ty1) (T.generalizeTy ty2) = return (Left ty2)
    checkTys ty1 ty2 = return $ Right (ErrTypeNeq loc (T.generalizeTy ty2) (T.generalizeTy ty1) vart)

tryCheckTerm (D.Mu loc v c) ty = do
  addCheckerVar v (shiftEvalOrder ty)
  c' <- tryCheckCommand c
  let leftFun cmd = T.Mu loc v cmd ty
  return $ first leftFun c' 

tryCheckTerm xtt@(D.Xtor loc xtn xtargs) ty@(T.TyDecl tyn tyargs (MkKind eo)) = do 
  T.MkData _ tyn' argVars _ _ <- lookupXtorDecl loc xtn 
  if tyn /= tyn' then return $ Right (ErrNotTyDecl loc tyn' ty xtt) else do
    let argEos = MkKind . (`varianceEvalOrder` eo) . variantVariance <$> argVars
    tyargsZipped <- zipWithError (getKind <$> tyargs)  argEos (ErrTypeArity loc tyn)
    unless (all (uncurry (==)) tyargsZipped) $ throwError (ErrArgumentKind loc tyn tyargs)
    T.MkXtorSig _ _ xtargs'  <- lookupXtor loc xtn
    varmap <- M.fromList <$> zipWithError (variantVar <$> argVars) tyargs (ErrTypeArity loc tyn)
    let xtargs'' = T.substTyVars varmap <$> xtargs'
    xtArgsZipped <- zipWithError xtargs xtargs'' (ErrXtorArity loc xtn)
    xtargs''' <- forM xtArgsZipped (uncurry tryCheckTerm)
    let (xtargs'''', errs) = liftEitherErrorList xtargs'''
    if null errs then return $ Left (T.Xtor loc xtn xtargs'''' ty) else return $ Right (ErrList loc errs)

tryCheckTerm xct@(D.XCase loc pts@(pt1:_)) ty@(T.TyDecl tyn tyargs (MkKind eo)) = do 
  T.MkData _ tyn' argVars _ xtors <- lookupXtorDecl loc (D.ptxt pt1)
  if tyn /= tyn' then return $ Right (ErrNotTyDecl loc tyn' ty xct) else do
    let argEos = MkKind . (`varianceEvalOrder` eo) . variantVariance <$> argVars
    tyArgsZipped <- zipWithError (getKind <$> tyargs) argEos (ErrTypeArity loc tyn)
    if not (all (uncurry (==)) tyArgsZipped) then return $ Right (ErrArgumentKind loc tyn tyargs) else do 
      let ptxtns = D.ptxt <$> pts
      let declxtns = T.sigName <$> xtors
      if not (all (`elem` ptxtns) declxtns) then return $ Right (ErrBadPattern loc ptxtns declxtns xct) else do 
        if not (all (`elem` declxtns) ptxtns) then return $ Right (ErrBadPattern loc ptxtns declxtns xct) else do
          varmap <- M.fromList <$> zipWithError (variantVar <$> argVars) tyargs (ErrTypeArity loc tyn)
          pts' <- forM pts (`checkPattern` varmap)
          let (pts'', errs) = liftEitherErrorList pts'
          if null errs then 
            return $ Left (T.XCase loc pts'' ty) else return $ Right (ErrList loc errs)
  where 
    checkPattern :: D.Pattern -> M.Map Typevar T.Ty -> CheckM (Either T.Pattern  CheckerError)
    checkPattern (D.MkPattern xtn vars c) varmap = do
      T.MkXtorSig _ _ xtargs <- lookupXtor loc xtn
      let xtargs' = T.substTyVars varmap <$> xtargs
      argsZipped <- zipWithError vars xtargs' (ErrXtorArity loc xtn)
      currVars <- getCheckerVars 
      let newVars = foldr (\(v,ty') m -> M.insert v ty' m)  currVars argsZipped 
      c' <- withCheckerVars newVars (tryCheckCommand c)
      let leftFun = T.MkPattern xtn vars
      return $ first leftFun c' 

tryCheckTerm shiftt@(D.ShiftPos loc _) shiftty@(T.TyShift ty (MkKind CBV)) | getKind ty /= MkKind CBN = 
  return $ Right (ErrKindNeq loc ty shiftty shiftt)
tryCheckTerm (D.ShiftPos loc t) (T.TyShift ty (MkKind CBV)) = do 
  t' <- tryCheckTerm t ty 
  case t' of 
    Left t'' -> return $ Left (T.ShiftPos loc t'' (T.TyShift (T.getType t'') (MkKind CBV)))
    Right err -> return $ Right err

tryCheckTerm shiftt@(D.ShiftNeg loc _ _) shiftty@(T.TyShift ty (MkKind CBN)) | getKind ty /= MkKind CBV = 
  return (Right (ErrKindNeq loc ty shiftty shiftt))
tryCheckTerm (D.ShiftNeg loc v c) (T.TyShift ty (MkKind CBN)) = do 
  addCheckerVar v ty
  c' <- tryCheckCommand c
  let leftFun cmd = T.ShiftNeg loc v cmd ty
  return $ first leftFun c' 
  
tryCheckTerm t ty = return (Right (ErrBadType (getLoc t) t ty))

------------------------------------------------------------------------------------
------------------------------------- Commands -------------------------------------
------------------------------------------------------------------------------------

checkCommand :: D.Command -> CheckM T.Command
checkCommand c = do 
  c' <- tryCheckCommand c 
  case c' of 
    Left c'' -> return c'' 
    Right err -> throwError err

tryCheckCommand :: D.Command -> CheckM (Either T.Command CheckerError)
tryCheckCommand c@(D.Cut loc (D.Var loc1 v1) pol (D.Var loc2 v2)) = do
  mty1 <- getMTypeVar v1 
  mty2 <- getMTypeVar v2 
  case (mty1,mty2) of
    (Nothing,Nothing) -> return $ Right (ErrUnclearType loc c)
    (Just ty1,Nothing) -> return $ Left (T.Cut loc (T.Var loc1 v1 ty1) pol (T.Var loc2 v2 (shiftEvalOrder ty1)))
    (Nothing,Just ty2) -> return $ Left (T.Cut loc (T.Var loc1 v1 (shiftEvalOrder ty2)) pol (T.Var loc2 v2 ty2))
    (Just ty1,Just ty2) ->  
      if shiftEvalOrder ty1 == ty2 then 
        return $ Left (T.Cut loc (T.Var loc1 v1 ty1) pol (T.Var loc2 v2 ty2)) else 
        return $ Right (ErrUnclearType loc c)
tryCheckCommand c@(D.Cut loc (D.Var loc1 v1) pol u) = do
  mty <- getMTypeVar v1 
  case mty of 
    Nothing -> return $ Right (ErrUnclearType loc c)
    Just ty -> do 
      u' <- tryCheckTerm u (shiftEvalOrder ty)
      let leftFun = T.Cut loc (T.Var loc1 v1 ty) pol
      return $ first leftFun u'
tryCheckCommand c@(D.Cut loc t pol (D.Var loc2 v2)) = do
  mty <- getMTypeVar v2 
  case mty of 
    Nothing -> return $ Right (ErrUnclearType loc c)
    Just ty -> do 
      t' <- tryCheckTerm t (shiftEvalOrder ty)
      let leftFun ter = T.Cut loc ter pol (T.Var loc2 v2 ty)
      return $ first leftFun t'
tryCheckCommand c@D.Cut{} = return $ Right (ErrUnclearType (getLoc c) c)
tryCheckCommand (D.CutAnnot loc t ty eo u) = do
  ty1 <- tryCheckType loc ty (MkKind CBV)
  ty2 <- tryCheckType loc ty (MkKind CBN) 
  case (ty1,ty2) of 
    (Right err1, Right err2) -> return $ Right (ErrList loc [err1,err2])
    (Right err,_) -> return $ Right err 
    (_, Right err) -> return $ Right err
    (Left ty1', Left ty2') -> do
      t1 <- tryCheckTerm t ty1' 
      t2 <- tryCheckTerm t ty2'
      u1 <- tryCheckTerm u  ty2' 
      u2 <- tryCheckTerm u ty1'
      case (t1,u1)of 
        (Left t1',Left u1') -> return $ Left (T.Cut loc t1' eo u1')
        (merr1,merr2) -> case (t2,u2) of 
          (Left t2',Left u2') -> return $ Left (T.Cut loc t2' eo u2')
          (merr3,merr4) -> return $ Right (ErrList loc (getErrs [merr1,merr2,merr3,merr4]))
  where 
    getErrs :: [Either a CheckerError] -> [CheckerError]
    getErrs [] = [] 
    getErrs (Left _:merrs) = getErrs merrs
    getErrs (Right err: merrs) = err : getErrs merrs


tryCheckCommand (D.Done loc) = return $ Left (T.Done loc)
tryCheckCommand (D.Err loc err) = return $ Left (T.Err loc err)
tryCheckCommand c@(D.Print loc (D.Var loc1 v)) = do
  mty <- getMTypeVar v 
  case mty of 
    Nothing -> return $ Right (ErrUnclearType loc c) 
    Just ty -> return $ Left (T.Print loc (T.Var loc1 v ty))
tryCheckCommand c@D.Print{} = throwError (ErrUnclearType (getLoc c) c)
tryCheckCommand (D.PrintAnnot loc t ty) = do
  ty' <- tryCheckType loc ty (MkKind CBV)
  ty'' <- tryCheckType loc ty (MkKind CBN)
  let leftFun= T.Print loc
  case (ty',ty'') of 
    (Left ty''',_) -> do
      t' <- tryCheckTerm t ty'''
      return $ first leftFun t'
    (_,Left ty''') -> do
      t' <- tryCheckTerm t ty'''
      return $ first leftFun t'
    (Right err1,Right err2) -> return $ Right (ErrList loc [err1,err2])
