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
import Embed.EmbedTyped () 
import Embed.Definition

import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import Data.Map qualified as M
import Data.List (find)


-- WIP, should not be working yet 
checkTerm :: D.Term -> T.Ty -> CheckM T.Term
checkTerm (D.Var v) ty = do
  vars <- gets checkVars 
  case M.lookup v vars of 
    Nothing -> throwError (ErrMissingVar v WhereCheck)
    Just ty' -> if T.isSubsumed ty' ty then return (T.Var v ty') else throwError (ErrTypeNeq (embed ty') (embed ty) WhereCheck)

checkTerm (D.Mu v c) ty = do
  addVar v ty
  c' <- checkCommand c
  return (T.Mu v c' ty)

checkTerm (D.Xtor xtn xtargs) (T.TyForall tyvars ty@(T.TyDecl tyn argTys pol)) = do 
  T.MkDataDecl tyn' _ pol' _ <- lookupXtorDecl xtn
  -- make sure type name and polarity are correct
  unless (tyn' == tyn) $ throwError (ErrNotTyDecl tyn ty WhereCheck)
  unless (pol == pol') $ throwError (ErrKind (MkKind pol') (MkKind pol) ShouldEq WhereCheck)
  zipped <- zipWithError xtargs argTys (ErrXtorArity xtn WhereCheck)
  xtargs' <- forM zipped (\(t,ty') -> checkTerm t (T.TyForall tyvars ty'))
  return (T.Xtor xtn xtargs' ty)
checkTerm (D.Xtor xtn _) (T.TyForall _ ty) = do 
  T.MkDataDecl tyn _ _ _<- lookupXtorDecl xtn
  throwError (ErrNotTyDecl tyn ty WhereCheck) 

checkTerm (D.XCase []) _ = throwError (ErrBadPattern [] WhereCheck)
checkTerm (D.XCase pts@(pt1:_)) (T.TyForall _tyvars ty@(T.TyDecl tyn argTys pol)) = do
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
  pts' <- forM zipped (uncurry checkPattern)
  return $ T.XCase pts' ty

checkTerm (D.XCase (pt1:_)) (T.TyForall _ ty) = do
  T.MkDataDecl tyn _ _ _ <- lookupXtorDecl (D.ptxt pt1)
  throwError (ErrNotTyDecl tyn ty WhereCheck)

checkTerm (D.Shift t) tys@(T.TyForall _ ty) = do 
  t' <- checkTerm t tys
  let pol = getKind t'
  if pol /= Pos then throwError (ErrKind (MkKind pol) (MkKind Pos) ShouldEq WhereCheck) else return (T.Shift t' ty)

checkTerm (D.Lam v c) (T.TyForall _tyargs (T.TyShift ty)) = do
  addVar v ty
  c' <- checkCommand c
  return $ T.Lam v c' (T.TyShift ty)

checkTerm (D.Lam _ _) (T.TyForall _ ty) = throwError (ErrNotTyShift ty WhereCheck)
checkTerm _ _ = error "not implemented (checkTerm)"

checkPattern :: D.Pattern -> [T.Ty] -> CheckM T.Pattern 
checkPattern (D.MkPattern xtn vars c) argTys = do 
  zipped <- zipWithError vars argTys (ErrXtorArity xtn WhereCheck)
  forM_ zipped (uncurry addVar)
  -- not sure what type we need there
  c' <- checkCommand c 
  forM_ vars remVar 
  return $ T.MkPattern xtn vars c'

checkCommand :: D.Command -> CheckM T.Command
checkCommand (D.Cut t pol u) = do 
  tys <- getTyCommand t u 
  t' <- checkTerm t tys
  u' <- checkTerm u tys 
  let pol1 = getKind t' 
  let pol2 = getKind u'
  when (pol1 == pol2) $ throwError (ErrKind (MkKind pol1) (MkKind pol2) ShouldEq WhereCheck)
  return $ T.Cut t' pol u'
checkCommand D.Done = return T.Done 

getTyCommand :: D.Term -> D.Term -> CheckM T.Ty
getTyCommand (D.Var v) _ = do 
  vars <- gets checkVars
  case M.lookup v vars of 
    Nothing -> throwError (ErrMissingVar v WhereCheck)
    Just ty -> return (T.generalize ty)
getTyCommand t1 t2@D.Var{} = getTyCommand t2 t1
getTyCommand t _ = throwError (ErrTypeAmbig t WhereCheck)

-- mu a.<cons(1,nil) | + | case(cons(x,y) => <x|+|a>, nil => Done)> : Nat
-- a : Nat to env
-- check <cons(1,nil) | + | case(cons(x,y) => <x|+|a>, nil => Done)> checks
-- we need cons(1,nil) :List(Nat) : List(Nat)
-- then cons(1,nil) : List(Nat)
-- case(...) : List(Nat) 
-- -> command checks
-- to check case has List(Nat) we also check <x|+|a>
-- needs x:Nat, a:Nat
-- x:Nat by Cons 
-- a:Nat by env
