module TypeCheck.Terms where 

import TypeCheck.Definition
import Syntax.Typed.Terms         qualified as T
import Syntax.Typed.Types         qualified as T
import Syntax.Typed.Program       qualified as T
import Syntax.Desugared.Terms     qualified as D
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


-- WIP, should not be working yet 
checkTerm :: D.Term -> T.Ty -> CheckM T.Term
--checkTerm t (T.TyForall tyvars ty) = do 
--  setForall tyvars 
--  t' <- checkTerm t ty
--  case t' of 
--    T.Var v ty' -> return $ T.Var v (T.TyForall tyvars ty')
--    T.Mu v c ty' -> return $ T.Mu v c (T.TyForall tyvars ty')
--    T.Xtor xtn xtargs ty' -> return $ T.Xtor xtn xtargs (T.TyForall tyvars ty') 
--    T.XCase pts ty' -> return $ T.XCase pts (T.TyForall tyvars ty')
--    T.Shift t'' ty' -> return $ T.Shift t'' (T.TyForall tyvars ty')
--    T.Lam v c ty' -> return $ T.Lam v c (T.TyForall tyvars ty') 
checkTerm t (T.TyCo ty) = do 
  t' <- checkTerm t ty
  case t' of 
    T.Var v ty' -> return $ T.Var v (T.TyCo ty')
    T.Xtor xtn xtargs ty' -> return $ T.Xtor xtn xtargs (T.TyCo ty')
    T.Mu v c ty' -> return $ T.Mu v c (T.TyCo ty')
    T.XCase pts ty' -> return $ T.XCase pts (T.TyCo ty')
    T.Shift{} -> throwError (ErrKind ShouldEq "checkTerm TyCo")
    T.Lam{} -> throwError (ErrKind ShouldEq "checkTerm TyCo")

checkTerm (D.Var v) ty = do
  vars <- gets checkVars 
  case M.lookup v vars of 
    Nothing -> throwError (ErrMissingVar v "checkTerm Var")
    Just ty' -> if ty' == ty then return (T.Var v ty) else throwError (ErrTypeNeq (embed ty') (embed ty) "checkTerm Var")

checkTerm (D.Mu v c) ty = do
  addVar v ty
  c' <- checkCommand c
  return (T.Mu v c' ty)

checkTerm (D.Xtor xtn xtargs) ty@(T.TyDecl tyn tyargs pol) = do 
  T.MkDataDecl tyn' tyargs' pol' _ <- lookupXtorDecl xtn 
  T.MkXtorSig _ xtargs'  <- lookupXtor xtn
  unless (tyn == tyn') $ throwError (ErrNotTyDecl tyn ty "checkTerm Xtor")
  unless (pol == pol') $ throwError (ErrKind ShouldEq "checkTerm Xtor")
  tyargsZipped <- zipWithError (getKind <$> tyargs) (getKind <$> tyargs') (ErrTyArity tyn "checkTerm Xtor")
  unless (all (uncurry (==)) tyargsZipped) $ throwError (ErrKind ShouldEq "checkTerm Xtor")
  xtArgsZipped <- zipWithError xtargs xtargs' (ErrXtorArity xtn "checkTerm Xtor")
  xtargs'' <- forM xtArgsZipped (uncurry checkTerm)
  return (T.Xtor xtn xtargs'' ty)

checkTerm (D.XCase pts@(pt1:_)) ty@(T.TyDecl tyn tyargs pol) = do 
  T.MkDataDecl tyn' argVars pol' xtors <- lookupXtorDecl (D.ptxt pt1)
  unless (tyn == tyn') $ throwError (ErrNotTyDecl tyn' ty "checkTerm XCase")
  unless (flipPol pol == pol') $ throwError (ErrKind ShouldEq (", " <> show ty <> " should have kind " <> show (flipPol pol) <> " checkTerm XCase"))
  let ptxtns = D.ptxt <$> pts
  let declxtns = T.sigName <$> xtors
  unless (all (`elem` declxtns) ptxtns) $ throwError (ErrBadPattern ptxtns "checkTerm XCase")
  tyArgsZip <- zipWithError (getKind <$> argVars) (getKind<$> tyargs) (ErrTyArity tyn "checkTerm XCase")
  allEqWithError tyArgsZip (ErrKind ShouldEq "checkTerm XCase")
  pts' <- forM pts checkPattern
  return $ T.XCase pts' ty 
  where 
    checkPattern :: D.Pattern -> CheckM T.Pattern 
    checkPattern (D.MkPattern xtn vars c) = do
      T.MkXtorSig _ xtargs' <- lookupXtor xtn
      argsZipped <- zipWithError vars xtargs' (ErrXtorArity xtn "checkTerm XCase" )
      forM_ argsZipped (uncurry addVar)
      c' <- checkCommand c
      forM_ vars remVar
      return $ T.MkPattern xtn vars c'

checkTerm (D.Shift t) (T.TyShift ty) = do 
  t' <- checkTerm t ty 
  unless (getKind t' == Pos) $ throwError (ErrKind ShouldEq "checkTerm Shift")
  case t' of 
    T.Shift t'' ty' -> return $ T.Shift t'' (T.TyShift ty') 
    badTerm -> throwError (ErrNotTyShift (T.getType badTerm) "checkTerm Shift")

checkTerm (D.Lam v c) (T.TyShift ty) = do 
  unless (getKind ty == Pos) $ throwError (ErrKind ShouldEq "checkTerm Shift")
  addVar v ty
  c' <- checkCommand c
  return $ T.Lam v c' ty
  
checkTerm t _ = throwError (ErrTypeAmbig t "checkterm other")


checkCommand :: D.Command -> CheckM T.Command
checkCommand (D.Cut t pol u) = do 
  ty <- getTyCommand t u 
  t' <- checkTerm t ty
  u' <- checkTerm u ty
  let pol1 = getKind t' 
  let pol2 = getKind u'
  when (pol1 == pol2) $ throwError (ErrKind ShouldEq "checkCommand")
  return $ T.Cut t' pol u'
checkCommand D.Done = return T.Done 

getTyCommand :: D.Term -> D.Term -> CheckM T.Ty
getTyCommand (D.Var v) _ = do 
  vars <- gets checkVars
  case M.lookup v vars of 
    Nothing -> throwError (ErrMissingVar v "checkCommand")
    Just ty -> return ty
getTyCommand t1 t2@D.Var{} = getTyCommand t2 t1
getTyCommand t _ = throwError (ErrTypeAmbig t "checkCommand")
