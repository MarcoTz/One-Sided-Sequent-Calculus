module TypeCheck.Terms (
  checkTerm,
  checkCommand
) where 

import TypeCheck.Definition
import TypeCheck.Types
import Syntax.Typed.Terms         qualified as T
import Syntax.Typed.Types         qualified as T
import Syntax.Typed.Program       qualified as T
import Syntax.Typed.Substitution  qualified as T
import Syntax.Desugared.Terms     qualified as D
import Environment
import Errors
import Common
import Embed.EmbedTyped () 
import Pretty.Typed ()

import Control.Monad
import Control.Monad.Except
import Data.Map qualified as M

checkTerm :: D.Term -> T.Ty -> CheckM T.Term
checkTerm t (T.TyForall args ty) = do 
  forM_ args addCheckerTyVar 
  t' <- checkTerm t ty
  case t' of 
    T.Var v ty' -> return $ T.Var v (T.TyForall args ty')
    T.Xtor xtn xtargs ty' -> return $ T.Xtor xtn xtargs (T.TyForall args ty')
    T.Mu v c ty' -> return $ T.Mu v c (T.TyForall args ty')
    T.XCase pts ty' -> return $ T.XCase pts (T.TyForall args ty')
    T.ShiftPos t'' ty' -> return $ T.ShiftPos t'' (T.TyForall args ty')
    T.ShiftNeg v c ty' -> return $ T.ShiftNeg v c (T.TyForall args ty')

checkTerm t (T.TyCo ty) = do 
  t' <- checkTerm t (flipPol ty)
  case t' of 
    T.Var v ty' -> return $ T.Var v (T.TyCo ty')
    T.Xtor xtn xtargs ty' -> return $ T.Xtor xtn xtargs (T.TyCo ty')
    T.Mu v c ty' -> return $ T.Mu v c (T.TyCo ty')
    T.XCase pts ty' -> return $ T.XCase pts (T.TyCo ty')
    T.ShiftPos{} -> throwError (ErrTyCoForShift t' ty)
    T.ShiftNeg{} -> throwError (ErrTyCoForShift t' ty)

checkTerm (D.Var v) ty = do
  vars <- getCheckerVars 
  mdecl <- lookupMVar v
  mrec <- lookupMRec v
  case (M.lookup v vars,mdecl,mrec) of 
    (Nothing,Nothing,Nothing) -> throwError (ErrUndefinedVar v)
    (Just (T.TyVar tyv pol),_,_) -> do
      unless (pol == getKind ty) $ throwError (ErrKindNeq (T.TyVar tyv pol) ty (D.Var v))
      tyVars <- getCheckerTyVars 
      if tyv `elem` tyVars then return $ T.Var v ty
      else throwError (ErrUndefinedTyVar tyv (D.Var v))
    (Just ty',_,_) -> T.Var v <$> checkTys ty ty'
    (_,Just (T.MkVar _ ty' _),_) ->  T.Var v <$> checkTys ty ty'
    (_,_,Just (T.MkRec _ ty' _)) ->  T.Var v <$> checkTys ty ty'
  where 
    checkTys :: T.Ty -> T.Ty -> CheckM T.Ty
    checkTys ty1 ty2 = do
      unless (getKind ty1 == getKind ty2) $ throwError (ErrKindNeq ty1 ty2 (D.Var v))
      if T.isSubsumed ty1 ty2 || T.isSubsumed ty2 ty1 then return ty2
      else throwError (ErrTypeNeq ty2 ty1 (D.Var v))

checkTerm (D.Mu v c) ty = do
  addCheckerVar v (flipPol ty)
  c' <- checkCommand c
  return (T.Mu v c' ty)

checkTerm xtt@(D.Xtor xtn xtargs) ty@(T.TyDecl tyn tyargs pol) = do 
  T.MkData tyn' argVars pol' _ <- lookupXtorDecl xtn 
  let kindErr = ErrKindNeq ty (T.TyDecl tyn ((\(MkPolVar v p) -> T.TyVar v p) <$> argVars) pol') xtt
  unless (pol' == pol) $ throwError kindErr 
  unless (tyn == tyn') $ throwError (ErrNotTyDecl tyn' (T.TyDecl tyn [] pol') xtt)
  tyargsZipped <- zipWithError (getKind <$> tyargs) (getKind <$> argVars) (ErrTypeArity tyn)
  unless (all (uncurry (==)) tyargsZipped) $ throwError kindErr 
  T.MkXtorSig _ xtargs'  <- lookupXtor xtn
  varmap <- M.fromList <$> zipWithError argVars tyargs (ErrTypeArity tyn)
  let xtargs'' = T.substTyVars varmap <$> xtargs'
  xtArgsZipped <- zipWithError xtargs xtargs'' (ErrXtorArity xtn)
  xtargs''' <- forM xtArgsZipped (uncurry checkTerm)
  return (T.Xtor xtn xtargs''' ty)

checkTerm xct@(D.XCase pts@(pt1:_)) ty@(T.TyDecl tyn tyargs pol) = do 
  T.MkData tyn' argVars pol' xtors <- lookupXtorDecl (D.ptxt pt1)
  let kindErr = ErrKindNeq (flipPol ty) (T.TyDecl tyn ((\(MkPolVar v p) -> T.TyVar v p) <$> argVars) pol')  xct
  unless (pol' /= pol) $ throwError kindErr
  unless (tyn == tyn') $ throwError (ErrNotTyDecl tyn' (T.TyDecl tyn [] pol') xct)
  tyArgsZipped <- zipWithError (getKind <$> tyargs) (getKind <$> argVars) (ErrTypeArity tyn)
  unless (all (uncurry (==)) tyArgsZipped) $ throwError kindErr
  let ptxtns = D.ptxt <$> pts
  let declxtns = T.sigName <$> xtors
  unless (all (`elem` ptxtns) declxtns) $ throwError (ErrBadPattern ptxtns declxtns xct)
  unless (all (`elem` declxtns) ptxtns) $ throwError (ErrBadPattern ptxtns declxtns xct)
  varmap <- M.fromList <$> zipWithError argVars tyargs (ErrTypeArity tyn)
  pts' <- forM pts (`checkPattern` varmap)
  return $ T.XCase pts' ty 
  where 
    checkPattern :: D.Pattern -> M.Map PolVar T.Ty -> CheckM T.Pattern 
    checkPattern (D.MkPattern xtn vars c) varmap = do
      T.MkXtorSig _ xtargs <- lookupXtor xtn
      let xtargs' = T.substTyVars varmap <$> xtargs
      argsZipped <- zipWithError vars xtargs' (ErrXtorArity xtn)
      currVars <- getCheckerVars 
      let newVars = foldr (\(v,ty') m -> M.insert v ty' m)  currVars argsZipped 
      c' <- withCheckerVars newVars (checkCommand c)
      return $ T.MkPattern xtn vars c'

checkTerm (D.ShiftPos t) (T.TyShift ty Pos) = do 
  t' <- checkTerm t ty 
  unless (getKind t' == Pos) $ throwError (ErrKindNeq (T.getType t') (T.TyShift ty Pos) (D.ShiftPos t))
  return $ T.ShiftPos t' (T.TyShift (T.getType t') Pos)

checkTerm (D.ShiftNeg v c) (T.TyShift ty Neg) = do 
  unless (getKind ty == Pos) $ throwError (ErrKindNeq ty (T.TyShift ty Neg) (D.ShiftNeg v c))
  addCheckerVar v ty
  c' <- checkCommand c
  return $ T.ShiftNeg v c' ty
  
checkTerm t ty = throwError (ErrBadType t ty)

checkCommand :: D.Command -> CheckM T.Command
checkCommand c@(D.Cut t pol u) = do 
  ty <- getTyCommand t u
  t' <- checkTerm t ty
  u' <- checkTerm u (flipPol ty)
  let pol1 = getKind t' 
  let pol2 = getKind u'
  when (pol1 == pol2) $ throwError (ErrCutKind (T.getType t') (T.getType u') c)
  return $ T.Cut t' pol u'
checkCommand c@(D.CutAnnot t ty pol u) = do
  ty' <- checkPolTy ty 
  ty'' <- checkPolTy (flipPol ty)
  t' <- checkTerm t ty' 
  u' <- checkTerm u  ty'' 
  let pol1 = getKind t' 
  let pol2 = getKind u' 
  when (pol1 == pol2) $ throwError (ErrCutKind (T.getType t') (T.getType u') c)
  return $ T.Cut t' pol u'
checkCommand D.Done = return T.Done 
checkCommand (D.Err err) = return $ T.Err err

getTyCommand :: D.Term -> D.Term -> CheckM T.Ty 
getTyCommand (D.Var v) _ = do 
  vars <- getCheckerVars 
  case M.lookup v vars of 
    Nothing -> throwError (ErrUndefinedVar v)
    Just ty -> return ty
getTyCommand t1 t2@D.Var{} = flipPol <$> getTyCommand t2 t1
getTyCommand t1 t2 = throwError (ErrUnclearTypeCut t1 t2)
