module TypeCheck.Terms where 

import TypeCheck.Definition
import TypeCheck.Types
import Syntax.Typed.Terms         qualified as T
import Syntax.Typed.Types         qualified as T
import Syntax.Typed.Program       qualified as T
import Syntax.Typed.Substitution  qualified as T
import Syntax.Desugared.Terms     qualified as D
import Syntax.Desugared.Types     qualified as D
import Environment
import Errors
import Common
import Embed.EmbedTyped () 
import Embed.Definition
import Pretty.Types ()

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
    T.ShiftPos{} -> throwError (ErrKind ShouldEq (T.getType t') (T.TyCo ty) "checkTerm TyCo")
    T.ShiftNeg{} -> throwError (ErrKind ShouldEq (T.getType t') (T.TyCo ty) "checkTerm TyCo")

checkTerm (D.Var v) ty = do
  vars <- gets checkVars
  mdecl <- lookupMVar v
  case (M.lookup v vars,mdecl) of 
    (Nothing,Nothing)   -> throwError (ErrMissingVar v "checkTerm Var")
    (Just (T.TyVar tyv pol),_) -> do
      unless (pol == getKind ty) $ throwError (ErrKind ShouldEq (T.TyVar tyv pol) ty "checkTerm Var")
      tyVars <- gets checkTyVars 
      if tyv `elem` tyVars then return $ T.Var v ty
      else throwError (ErrMissingTyVar tyv "checkTerm Var")
    (Just ty',_) -> 
      if ty' == ty then return (T.Var v ty') 
      else throwError (ErrTypeNeq (embed ty') (embed ty) ("checkTerm Var (checkVars), variable " <> show v))
    (_,Just (T.MkVar _ ty' _)) -> 
      if ty' == ty then return (T.Var v ty') 
      else throwError (ErrTypeNeq (embed ty') (embed ty) ("checkTerm Var (declvars), variable " <> show v))

checkTerm (D.Mu v c) ty = do
  addVarPol v ty
  c' <- checkCommand c
  return (T.Mu v c' ty)

checkTerm (D.Xtor xtn xtargs) ty@(T.TyDecl tyn tyargs pol) = do 
  T.MkData tyn' argVars pol' _ <- lookupXtorDecl xtn 
  let kindErr = ErrKind ShouldEq ty (T.TyDecl tyn ((\(MkPolVar v p) -> T.TyVar v p) <$> argVars) pol') "checkTerm Xtor"
  unless (pol' == pol) $ throwError kindErr
  unless (tyn == tyn') $ throwError (ErrNotTyDecl tyn' (T.TyDecl tyn [] pol') "checkTerm Xtor")
  tyargsZipped <- zipWithError (getKind <$> tyargs) (getKind <$> argVars) (ErrTyArity tyn "checkTerm xtor")
  unless (all (uncurry (==)) tyargsZipped) $ throwError kindErr 
  T.MkXtorSig _ xtargs'  <- lookupXtor xtn
  varmap <- M.fromList <$> zipWithError argVars tyargs (ErrTyArity tyn "checkTerm Xtor")
  let xtargs'' = T.substTyVars varmap <$> xtargs'
  xtArgsZipped <- zipWithError xtargs xtargs'' (ErrXtorArity xtn "checkTerm Xtor")
  xtargs''' <- forM xtArgsZipped (uncurry checkTerm)
  return (T.Xtor xtn xtargs''' ty)

checkTerm (D.XCase pts@(pt1:_)) ty@(T.TyDecl tyn tyargs pol) = do 
  T.MkData tyn' argVars pol' xtors <- lookupXtorDecl (D.ptxt pt1)
  let kindErr = ErrKind ShouldNeq ty (T.TyDecl tyn ((\(MkPolVar v p) -> T.TyVar v p) <$> argVars) pol') "checkTerm XCase"
  unless (pol' /= pol) $ throwError kindErr 
  unless (tyn == tyn') $ throwError (ErrNotTyDecl tyn' (T.TyDecl tyn [] pol') "checkTerm XCase")
  tyArgsZipped <- zipWithError (getKind <$> tyargs) (getKind <$> argVars) (ErrTyArity tyn "checkTerm xcase")
  unless (all (uncurry (==)) tyArgsZipped) $ throwError kindErr 
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

-- tyshift is not in desugared yet
--checkTerm (D.Shift t) (D.TyShift ty) = do 
--  t' <- checkTerm t ty 
--  unless (getKind t' == Pos) $ throwError (ErrKind ShouldEq "checkTerm Shift")
--  case t' of 
--    T.Shift t'' ty' -> return $ T.Shift t'' (T.TyShift ty') 
--    badTerm -> throwError (ErrNotTyShift (T.getType badTerm) "checkTerm Shift")

--checkTerm (D.Lam v c) (T.TyShift ty) = do 
--  unless (getKind ty == Pos) $ throwError (ErrKind ShouldEq "checkTerm Shift")
--  addVar v ty
--  c' <- checkCommand c
--  return $ T.Lam v c' ty
  
checkTerm t ty = throwError (ErrTypeAmbig t ("checkterm other, type to check "<> show ty))


checkCommand :: D.Command -> CheckM T.Command
checkCommand (D.Cut t pol u) = do 
  ty <- getTyCommand t u
  ty' <- checkType ty pol 
  ty'' <- checkType ty (flipPol pol)
  t' <- checkTerm t ty'
  u' <- checkTerm u ty''
  let pol1 = getKind t' 
  let pol2 = getKind u'
  when (pol1 == pol2) $ throwError (ErrKind ShouldNeq (T.getType t') (T.getType u') "checkCommand cut")
  return $ T.Cut t' pol u'
checkCommand (D.CutAnnot t ty pol u) = do
  ty' <- checkType ty pol
  ty'' <- checkType ty (flipPol pol)
  t' <- checkTerm t ty' 
  u' <- checkTerm u  ty'' 
  let pol1 = getKind t' 
  let pol2 = getKind u' 
  when (pol1 == pol2) $ throwError (ErrKind ShouldNeq (T.getType t') (T.getType u') "checkCommand annot")
  return $ T.Cut t' pol u'
checkCommand D.Done = return T.Done 

getTyCommand :: D.Term -> D.Term -> CheckM D.Ty
getTyCommand (D.Var v) _ = do 
  vars <- gets checkVars
  case M.lookup v vars of 
    Nothing -> throwError (ErrMissingVar v "checkCommand")
    Just ty -> return (embed ty)
getTyCommand t1 t2@D.Var{} = getTyCommand t2 t1
getTyCommand t _ = throwError (ErrTypeAmbig t "checkCommand")
