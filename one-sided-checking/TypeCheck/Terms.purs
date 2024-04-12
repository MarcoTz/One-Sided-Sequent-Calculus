module TypeCheck.Terms (
  checkTerm,
  checkCommand
) where 

import TypeCheck.Definition
import TypeCheck.Types
import Syntax.Typed.Terms         qualified as T
import Syntax.Typed.Types         qualified as T
import Syntax.Kinded.Program      qualified as K
import Syntax.Typed.Substitution  qualified as T
import Syntax.Desugared.Terms     qualified as D
import Embed.Definition
import Embed.EmbedKinded ()
import Environment
import Errors
import Common
import Loc
import Embed.EmbedTyped () 
import Pretty.Typed ()

import Control.Monad
import Control.Monad.Except
import Data.Map qualified as M

-----------------------------------------------------------------------------
----------------------------------- Terms -----------------------------------
-----------------------------------------------------------------------------

checkTerm :: D.Term -> T.Ty -> CheckM T.Term 
checkTerm t (T.TyForall args ty) = do 
  forM_ args addCheckerTyVar 
  t' <- checkTerm t ty
  let newTy = T.TyForall args (T.getType t')
  return $ T.setType t' newTy

checkTerm t (T.TyCo ty) = do 
  t' <- checkTerm t ty 
  let newTy = T.TyCo (T.getType t')
  return $ T.setType t' newTy 

checkTerm (D.Var loc v) ty = do
  checkerTy <- M.lookup v <$> getCheckerVars 
  vardecl <- lookupMVar v
  recdecl <- lookupMRec v
  let mty = firstJust [checkerTy,embed . K.varTy <$> vardecl,embed . K.recTy <$> recdecl]
  case mty of 
    Nothing -> throwError (ErrUndefinedVar loc v)
    Just ty' -> if T.isSubsumed ty ty' then return $ T.Var loc v ty else throwError (ErrNotSubsumed loc ty ty')

checkTerm (D.Mu loc v c) ty = do
  addCheckerVar v ty 
  c' <- checkCommand c
  return $ T.Mu loc v c' ty

checkTerm xtt@(D.Xtor loc xtn xtargs) ty@(T.TyDecl tyn tyargs) = do 
  K.MkData _ tyn' argVars _ _ <- lookupXtorDecl loc xtn 
  K.MkXtorSig _ _ xtargs'  <- lookupXtor loc xtn
  unless (tyn == tyn') $  throwError (ErrNotTyDecl loc tyn' ty xtt) 
  -- substitute type variables in xtor signature
  varSubsts <- zipWithErrorM (variantVar <$> argVars) tyargs (ErrTypeArity loc tyn)
  let xtArgTys = T.substTyvars (M.fromList varSubsts) . embed <$> xtargs'
  -- check xtor arguments
  argsToCheck <- zipWithErrorM xtargs xtArgTys (ErrXtorArity loc xtn) 
  argsChecked <- forM argsToCheck (uncurry checkTerm)
  return $ T.Xtor loc xtn argsChecked ty


checkTerm xct@(D.XCase loc pts@(pt1:_)) ty@(T.TyDecl tyn tyargs ) = do 
  K.MkData _ tyn' argVars _ xtors <- lookupXtorDecl loc (D.ptxt pt1)
  unless (tyn == tyn') $ throwError (ErrNotTyDecl loc tyn' ty xct) 
  -- make sure the correct xtors are present in patterns
  let ptxtns = D.ptxt <$> pts
  let declxtns = K.sigName <$> xtors
  unless (all (`elem` ptxtns) declxtns) $ throwError (ErrBadPattern loc ptxtns declxtns xct)
  unless (all (`elem` declxtns) ptxtns) $ throwError (ErrBadPattern loc ptxtns declxtns xct)
  varmap <- zipWithErrorM (variantVar <$> argVars) tyargs (ErrTypeArity loc tyn)
  pts' <- forM pts (`checkPattern` M.fromList varmap)
  return $ T.XCase loc pts' ty
  where 
    checkPattern :: D.Pattern -> M.Map Typevar T.Ty -> CheckM T.Pattern
    checkPattern (D.MkPattern xtn vars c) varmap = do
      K.MkXtorSig _ _ xtargs <- lookupXtor loc xtn
      let xtargs' = T.substTyvars varmap . embed <$> xtargs
      argsZipped <- zipWithErrorM vars xtargs' (ErrXtorArity loc xtn)
      currVars <- getCheckerVars 
      let newVars = foldr (\(v,ty') m -> M.insert v ty' m)  currVars argsZipped
      c' <- withCheckerVars newVars (checkCommand c)
      return $ T.MkPattern xtn vars c' 

checkTerm (D.ShiftCBV loc t) (T.TyShift ty) = do 
  t' <- checkTerm t ty 
  let newTy = T.TyShift (T.getType t') 
  return $ T.ShiftCBV loc t' newTy 

checkTerm (D.ShiftCBN loc t) (T.TyShift ty) = do 
  t' <- checkTerm t ty 
  let newTy = T.TyShift (T.getType t') 
  return $ T.ShiftCBN loc t' newTy
  
checkTerm t ty = throwError (ErrBadType (getLoc t) t ty)

------------------------------------------------------------------------------------
------------------------------------- Commands -------------------------------------
------------------------------------------------------------------------------------

checkCommand :: D.Command -> CheckM T.Command 
checkCommand c@(D.Cut loc (D.Var loc1 v1) pol (D.Var loc2 v2)) = do
  mty1 <- getMTypeVar v1 
  mty2 <- getMTypeVar v2 
  case (mty1,mty2) of
    (Nothing,Nothing)  -> throwError (ErrUnclearType loc c)
    (Just ty1,Nothing) -> return $ T.Cut loc (T.Var loc1 v1 ty1) pol (T.Var loc2 v2 ty1)
    (Nothing,Just ty2) -> return $ T.Cut loc (T.Var loc1 v1 ty2) pol (T.Var loc2 v2 ty2)
    (Just ty1, Just ty2) -> if ty1 == ty2 then return $ T.Cut loc (T.Var loc1 v1 ty1) pol (T.Var loc2 v2 ty2) else throwError (ErrUnclearType loc c)
checkCommand (D.Cut loc (D.Var loc1 v1) eo u) = do
  ty <- getTypeVar loc1 v1 
  u' <- checkTerm u ty
  return $ T.Cut loc (T.Var loc1 v1 ty) eo u' 
checkCommand (D.Cut loc t eo (D.Var loc2 v2)) = do
  ty <- getTypeVar loc2 v2 
  t' <- checkTerm t ty
  return $ T.Cut loc t' eo (T.Var loc2 v2 ty) 
checkCommand c@D.Cut{} = throwError (ErrUnclearType (getLoc c) c)
checkCommand (D.CutAnnot loc t ty eo u) = do
  ty1 <- checkType loc ty
  ty2 <- checkType loc ty 
  t' <- checkTerm t ty1
  u' <- checkTerm u  ty2
  return $ T.Cut loc t' eo u'

checkCommand (D.Done loc) = return $ T.Done loc
checkCommand (D.Err loc err) = return $ T.Err loc err
checkCommand (D.Print loc (D.Var loc1 v)) = do
  ty <- getTypeVar loc1 v 
  return $ T.Print loc (T.Var loc1 v ty)
checkCommand c@D.Print{} = throwError (ErrUnclearType (getLoc c) c)
checkCommand (D.PrintAnnot loc t ty) = do
  ty' <- checkType loc ty 
  t' <- checkTerm t ty'
  return $ T.Print loc t'
