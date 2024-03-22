module TypeCheck.Terms where 

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
import Embed.Definition
import Pretty.Typed ()

import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import Data.Map qualified as M

checkTerm :: D.Term -> T.Ty -> CheckM T.Term
checkTerm t (T.TyForall args ty) = do 
  forM_ args addTyVar 
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
    T.ShiftPos{} -> throwError (ErrKind ShouldEq (embed $ T.getType t') (embed $ T.TyCo ty) "checkTerm TyCo")
    T.ShiftNeg{} -> throwError (ErrKind ShouldEq (embed $ T.getType t') (embed $ T.TyCo ty) "checkTerm TyCo")

checkTerm (D.Var v) ty = do
  vars <- gets checkVars
  mdecl <- lookupMVar v
  mrec <- lookupMRec v
  case (M.lookup v vars,mdecl,mrec) of 
    (Nothing,Nothing,Nothing) -> throwError (ErrMissingVar v "checkTerm Var")
    (Just (T.TyVar tyv pol),_,_) -> do
      unless (pol == getKind ty) $ throwError (ErrKind ShouldEq (embed $ T.TyVar tyv pol) (embed ty) "checkTerm Var")
      tyVars <- gets checkTyVars 
      if tyv `elem` tyVars then return $ T.Var v ty
      else throwError (ErrMissingTyVar tyv "checkTerm Var")
    (Just ty',_,_) -> T.Var v <$> checkTys ty ty'
    (_,Just (T.MkVar _ ty' _),_) ->  T.Var v <$> checkTys ty ty'
    (_,_,Just (T.MkRec _ ty' _)) ->  T.Var v <$> checkTys ty ty'
  where 
    checkTys :: T.Ty -> T.Ty -> CheckM T.Ty
    checkTys ty1 ty2 = do
      unless (getKind ty1 == getKind ty2) $ throwError (ErrKind ShouldEq (embed ty1) (embed ty2) ("checkTerm var " <> show v))
      if T.isSubsumed ty1 ty2 || T.isSubsumed ty2 ty1 then return ty2
      else throwError (ErrTypeNeq (embed ty2) (embed ty1) ("checkTerm Var" <> show v))

checkTerm (D.Mu v c) ty = do
  addVarPol v (flipPol ty)
  c' <- checkCommand c
  return (T.Mu v c' ty)

checkTerm (D.Xtor xtn xtargs) ty@(T.TyDecl tyn tyargs pol) = do 
  T.MkData tyn' argVars pol' _ <- lookupXtorDecl xtn 
  let kindErr = ErrKind ShouldEq (embed ty) (embed $ T.TyDecl tyn ((\(MkPolVar v p) -> T.TyVar v p) <$> argVars) pol') 
  unless (pol' == pol) $ throwError (kindErr ("kind of declaration " <> show pol' <> " and of type to check " <> show pol <> ", checkTerm Xtor" ))
  unless (tyn == tyn') $ throwError (ErrNotTyDecl tyn' (embed $ T.TyDecl tyn [] pol') "checkTerm Xtor")
  tyargsZipped <- zipWithError (getKind <$> tyargs) (getKind <$> argVars) (ErrTyArity tyn "checkTerm xtor")
  unless (all (uncurry (==)) tyargsZipped) $ throwError (kindErr ("type arguments " <> show tyargsZipped))
  T.MkXtorSig _ xtargs'  <- lookupXtor xtn
  varmap <- M.fromList <$> zipWithError argVars tyargs (ErrTyArity tyn "checkTerm Xtor")
  let xtargs'' = T.substTyVars varmap <$> xtargs'
  xtArgsZipped <- zipWithError xtargs xtargs'' (ErrXtorArity xtn "checkTerm Xtor")
  xtargs''' <- forM xtArgsZipped (uncurry checkTerm)
  return (T.Xtor xtn xtargs''' ty)

checkTerm (D.XCase pts@(pt1:_)) ty@(T.TyDecl tyn tyargs pol) = do 
  T.MkData tyn' argVars pol' xtors <- lookupXtorDecl (D.ptxt pt1)
  let kindErr = ErrKind ShouldNeq (embed ty) (embed $ T.TyDecl tyn ((\(MkPolVar v p) -> T.TyVar v p) <$> argVars) pol') 
  unless (pol' /= pol) $ throwError (kindErr "checkTerm XCase")
  unless (tyn == tyn') $ throwError (ErrNotTyDecl tyn' (embed $ T.TyDecl tyn [] pol') "checkTerm XCase")
  tyArgsZipped <- zipWithError (getKind <$> tyargs) (getKind <$> argVars) (ErrTyArity tyn "checkTerm xcase")
  unless (all (uncurry (==)) tyArgsZipped) $ throwError (kindErr  ("checkTerm XCase  type arguments " <> show tyArgsZipped))
  let ptxtns = D.ptxt <$> pts
  let declxtns = T.sigName <$> xtors
  unless (all (`elem` declxtns) ptxtns) $ throwError (ErrBadPattern ptxtns "checkTerm XCase")
  varmap <- M.fromList <$> zipWithError argVars tyargs (ErrTyArity tyn "checkTerm XCase")
  pts' <- forM pts (`checkPattern` varmap)
  return $ T.XCase pts' ty 
  where 
    checkPattern :: D.Pattern -> M.Map PolVar T.Ty -> CheckM T.Pattern 
    checkPattern (D.MkPattern xtn vars c) varmap = do
      T.MkXtorSig _ xtargs <- lookupXtor xtn
      let xtargs' = T.substTyVars varmap <$> xtargs
      argsZipped <- zipWithError vars xtargs' (ErrXtorArity xtn "checkTerm XCase" )
      currVars <- gets checkVars
      let newVars = foldr (\(v,ty') m -> M.insert v ty' m)  currVars argsZipped 
      modify (MkCheckState newVars . checkTyVars) 
      c' <- checkCommand c
      modify (MkCheckState currVars . checkTyVars)
      return $ T.MkPattern xtn vars c'

checkTerm (D.ShiftPos t) (T.TyShift ty Pos) = do 
  t' <- checkTerm t ty 
  unless (getKind t' == Pos) $ throwError (ErrKind ShouldEq (embed $ T.getType t') (embed $ T.TyShift ty Pos) "checkTerm Shift")
  return $ T.ShiftPos t' (T.TyShift (T.getType t') Pos)

checkTerm (D.ShiftNeg v c) (T.TyShift ty Neg) = do 
  unless (getKind ty == Pos) $ throwError (ErrKind ShouldEq (embed ty) (embed (flipPol ty)) "checkTerm Shift")
  addVarPol v ty
  c' <- checkCommand c
  return $ T.ShiftNeg v c' ty
  
checkTerm t ty = throwError (ErrTypeAmbig (embed t) ("checkterm other, type to check "<> show ty))

checkCommand :: D.Command -> CheckM T.Command
checkCommand (D.Cut t pol u) = do 
  ty <- getTyCommand t u
  t' <- checkTerm t ty
  u' <- checkTerm u (flipPol ty)
  let pol1 = getKind t' 
  let pol2 = getKind u'
  when (pol1 == pol2) $ throwError (ErrKind ShouldNeq (embed $ T.getType t') (embed $ T.getType u') "checkCommand cut")
  return $ T.Cut t' pol u'
checkCommand (D.CutAnnot t ty pol u) = do
  ty' <- checkPolTy ty 
  ty'' <- checkPolTy (flipPol ty)
  t' <- checkTerm t ty' 
  u' <- checkTerm u  ty'' 
  let pol1 = getKind t' 
  let pol2 = getKind u' 
  when (pol1 == pol2) $ throwError (ErrKind ShouldNeq (embed $ T.getType t') (embed $ T.getType u') "checkCommand annot")
  return $ T.Cut t' pol u'
checkCommand D.Done = return T.Done 
checkCommand (D.Err err) = return $ T.Err err

getTyCommand :: D.Term -> D.Term -> CheckM T.Ty 
getTyCommand (D.Var v) _ = do 
  vars <- gets checkVars
  case M.lookup v vars of 
    Nothing -> throwError (ErrMissingVar v "checkCommand")
    Just ty -> return ty
getTyCommand t1 t2@D.Var{} = flipPol <$> getTyCommand t2 t1
getTyCommand t _ = throwError (ErrTypeAmbig (embed t) "checkCommand")
