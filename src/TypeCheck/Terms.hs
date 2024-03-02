module TypeCheck.Terms where 

import TypeCheck.Definition
import Syntax.Typed.Terms     qualified as T
import Syntax.Typed.Types     qualified as T
import Syntax.Typed.Program   qualified as T
import Syntax.Typed.Substitution qualified as T
import Syntax.Desugared.Terms qualified as D
import Environment
import Errors
import Utils
import Common

import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import Data.Map qualified as M
import Data.List (find)


-- WIP, should not be working yet 
checkTerm :: D.Term -> T.TypeScheme -> CheckM T.Term
checkTerm (D.Var v) tys = do
  vars <- gets checkVars 
  let ty = (\(T.MkTypeScheme _ t) -> t) tys
  case M.lookup v vars of 
    Nothing -> throwError (ErrMissingVar v WhereCheck)
    Just ty' -> if ty' == ty then return (T.Var v ty') else throwError (ErrTypeNeq ty' ty' WhereCheck)

checkTerm (D.Mu v c) tys = do
  let ty = (\(T.MkTypeScheme _ t) -> t) tys
  addVar v ty
  c' <- checkCommand c tys
  return (T.Mu v c' ty)

checkTerm (D.Xtor xtn xtargs) (T.MkTypeScheme tyvars ty@(T.TyDecl tyn argTys pol)) = do 
  T.MkDataDecl tyn' _ pol' _ <- lookupXtorDecl xtn
  -- make sure type name and polarity are correct
  unless (tyn' == tyn) $ throwError (ErrNotTyDecl tyn ty WhereCheck)
  unless (pol == pol') $ throwError (ErrKind (MkKind pol') (MkKind pol) ShouldEq WhereCheck)
  forM_ tyvars addTyVar
  zipped <- zipWithError xtargs argTys (ErrXtorArity xtn WhereCheck)
  xtargs' <- forM zipped (\(t,ty') -> checkTerm t (T.MkTypeScheme [] ty'))
  let newTy = T.TyDecl tyn (T.getType <$> xtargs') pol
  forM_ tyvars remTyVar
  return (T.Xtor xtn xtargs' newTy)
checkTerm (D.Xtor xtn _) (T.MkTypeScheme _ ty) = do 
  T.MkDataDecl tyn _ _ _<- lookupXtorDecl xtn
  throwError (ErrNotTyDecl tyn ty WhereCheck) 

checkTerm (D.XCase []) _ = throwError (ErrBadPattern [] WhereCheck)
checkTerm (D.XCase pts@(pt1:_)) (T.MkTypeScheme tyvars ty@(T.TyDecl tyn argTys pol)) = do
  T.MkDataDecl tyn' tyArgs pol' xtors <- lookupXtorDecl (D.ptxt pt1)
  -- check type name and polarity are correct
  unless (tyn == tyn') $ throwError (ErrNotTyDecl tyn ty WhereCheck)
  unless (pol == pol') $ throwError (ErrKind (MkKind pol) (MkKind pol') ShouldEq WhereCheck)
  let ptxts = D.ptxt <$> pts
  let declxts = T.sigName <$> xtors
  -- check the right xtors are used
  unless (all (`elem` ptxts) declxts) $ throwError (ErrBadPattern ptxts WhereCheck)
  unless (all (`elem` declxts) ptxts)  $ throwError (ErrBadPattern ptxts WhereCheck)
  let xtSigs = (\xt -> find (\x -> T.sigName x == xt) xtors) <$> ptxts
  xtSigs' <- forM xtSigs (`fromMaybeWithError` ErrBadPattern ptxts WhereCheck)
  subst <- M.fromList <$> zipWithError tyArgs argTys (ErrTyArity tyn WhereCheck)
  let xtArgTys = (\(T.MkXtorSig _ args) -> T.substTyVars subst <$> args) <$> xtSigs'
  zipped <- zipWithError pts xtArgTys (ErrBadPattern (D.ptxt <$> pts) WhereCheck)
  forM_ tyvars addTyVar
  pts' <- forM zipped (uncurry checkPattern)
  let newTy = T.TyDecl tyn argTys pol
  forM_ tyvars remTyVar
  return $ T.XCase pts' newTy

checkTerm (D.XCase (pt1:_)) (T.MkTypeScheme _ ty) = do
  T.MkDataDecl tyn _ _ _ <- lookupXtorDecl (D.ptxt pt1)
  throwError (ErrNotTyDecl tyn ty WhereCheck)

checkTerm (D.Shift t) tys@(T.MkTypeScheme tyargs ty) = do 
  forM_ tyargs addTyVar
  t' <- checkTerm t tys
  let pol = getKind t'
  forM_ tyargs remTyVar
  if pol /= Pos then throwError (ErrKind (MkKind pol) (MkKind Pos) ShouldEq WhereCheck) else return (T.Shift t' ty)

checkTerm (D.Lam v c) tys@(T.MkTypeScheme tyargs (T.TyShift ty pol)) = do
  when (pol /= Neg) $ throwError (ErrKind (MkKind pol) (MkKind Neg) ShouldEq WhereCheck)
  forM_ tyargs addTyVar 
  addVar v ty
  c' <- checkCommand c tys
  forM_ tyargs remTyVar
  return $ T.Lam v c' (T.TyShift ty Neg)

checkTerm (D.Lam _ _) (T.MkTypeScheme _ ty) = throwError (ErrNotTyShift ty WhereCheck)


checkCommand :: D.Command -> T.TypeScheme -> CheckM T.Command
checkCommand (D.Cut t pol u) tys = do 
  t' <- checkTerm t tys 
  u' <- checkTerm u tys
  let pol1 = getKind t' 
  let pol2 = getKind u'
  when (pol1 == pol2) $ throwError (ErrKind (MkKind pol1) (MkKind pol2) ShouldEq WhereCheck)
  return $ T.Cut t' pol u'
checkCommand D.Done _ = return T.Done 

checkPattern :: D.Pattern -> [T.Ty] -> CheckM T.Pattern 
checkPattern (D.MkPattern xtn vars c) argTys = do 
  zipped <- zipWithError vars argTys (ErrXtorArity xtn WhereCheck)
  forM_ zipped (uncurry addVar)
  -- not sure what type we need there
  c' <- checkCommand c (T.MkTypeScheme [] (T.TyVar (MkTypeVar "") Neg))
  forM_ vars remVar 
  return $ T.MkPattern xtn vars c'
