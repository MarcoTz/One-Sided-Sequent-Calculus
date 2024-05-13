module TypeCheck.Terms (
  checkTerm,
  checkCommand
) where 


import TypeCheck.Definition (CheckM, addCheckerTyVar, getCheckerVars, addCheckerVar,withCheckerVars,getMTypeVar,getTypeVar)
import TypeCheck.Types (checkType)
import TypeCheck.Errors (CheckerError(..))
import Common (Typevar,VariantVar(..))
import Loc (getLoc)
import Environment (lookupMVar,lookupXtorDecl, lookupXtor)
import Errors (zipWithErrorM)
import Syntax.Desugared.Terms (Term(..),Pattern(..),Command(..))                as D 
import Syntax.Typed.Terms (Term(..),Pattern(..),getType, setType, Command(..))  as T
import Syntax.Typed.Types (Ty(..),isSubsumed)                                   as T 
import Syntax.Typed.Substitution (substTyvars)                                  as T
import Syntax.Kinded.Program (VarDecl(..),DataDecl(..),XtorSig(..))             as K
import Syntax.Kinded.Types (embedType)
import Syntax.Kinded.Terms (getType)

import Prelude (bind,pure,($),(<$>), (==))
import Data.List (List(..),foldr)
import Data.Traversable (for)
import Data.Map (Map,lookup,fromFoldable,insert)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Set (fromFoldable) as S
import Control.Monad (unless)
import Control.Monad.Except (throwError)

-----------------------------------------------------------------------------
----------------------------------- Terms -----------------------------------
-----------------------------------------------------------------------------

checkTerm :: D.Term -> T.Ty -> CheckM T.Term 
checkTerm t (T.TyForall args ty) = do 
  _ <- for args addCheckerTyVar 
  t' <- checkTerm t ty
  let newTy = T.TyForall args (T.getType t')
  pure $ T.setType t' newTy

checkTerm t (T.TyCo ty) = do 
  t' <- checkTerm t ty 
  let newTy = T.TyCo (T.getType t')
  pure $ T.setType t' newTy 

checkTerm (D.Var loc v) ty = do
  checkerTy <- lookup v <$> getCheckerVars 
  vardecl <- lookupMVar v
  let mvarty = (\(K.VarDecl d) -> embedType (getType d.varBody)) <$> vardecl
  ty' <- case Tuple checkerTy mvarty of 
      (Tuple Nothing Nothing) -> throwError (ErrUndefinedVar loc v)
      (Tuple _ (Just ty'')) -> pure ty'' 
      (Tuple (Just ty'') _) -> pure ty'' 
  if T.isSubsumed ty ty' then pure (T.Var loc v ty) else throwError (ErrNotSubsumed loc ty ty')

checkTerm (D.Mu loc v c) ty = do
  _ <- addCheckerVar v ty 
  c' <- checkCommand c
  pure $ T.Mu loc v c' ty

checkTerm xtt@(D.Xtor loc xtn xtargs) ty@(T.TyDecl tyn tyargs) = do 
  K.DataDecl decl <- lookupXtorDecl loc xtn 
  K.XtorSig sig  <- lookupXtor loc xtn
  _ <- unless (tyn == decl.declName) $  throwError (ErrNotTyDecl loc decl.declName ty xtt) 
  -- substitute type variables in xtor signature
  let argVars = (\(VariantVar d) -> d.variantVar) <$> decl.declArgs
  varSubsts <- zipWithErrorM argVars tyargs (ErrTypeArity loc tyn)
  let argsEmbedded = embedType <$> sig.sigArgs
  let xtArgTys = T.substTyvars (fromFoldable varSubsts) <$> argsEmbedded 
  -- check xtor arguments
  argsToCheck <- zipWithErrorM xtargs xtArgTys (ErrXtorArity loc xtn) 
  argsChecked <- for argsToCheck (\(Tuple x y) -> checkTerm x y)
  pure $ T.Xtor loc xtn argsChecked ty


checkTerm xct@(D.XCase loc pts@(Cons (D.Pattern pt1) _)) ty@(T.TyDecl tyn tyargs ) = do 
  K.DataDecl decl <- lookupXtorDecl loc pt1.ptxt
  _ <- unless (tyn == decl.declName) $ throwError (ErrNotTyDecl loc decl.declName ty xct) 
  -- make sure the correct xtors are present in patterns
  let ptxtns = (\(D.Pattern pt) -> pt.ptxt) <$> pts
  let declxtns = (\(K.XtorSig sig) -> sig.sigName) <$> decl.declXtors 
  _ <- unless (S.fromFoldable ptxtns == S.fromFoldable declxtns) $ throwError (ErrBadPattern loc ptxtns declxtns xct)
  varmap <- zipWithErrorM ((\(VariantVar v) -> v.variantVar) <$> decl.declArgs) tyargs (ErrTypeArity loc tyn)
  pts' <- for pts (\x -> x `checkPattern` fromFoldable varmap)
  pure $ T.XCase loc pts' ty
  where 
    checkPattern :: D.Pattern -> Map Typevar T.Ty -> CheckM T.Pattern
    checkPattern (D.Pattern pt) varmap = do
      K.XtorSig sig <- lookupXtor loc pt.ptxt
      let argsEmbedded = embedType <$> sig.sigArgs
      let xtargs' = T.substTyvars varmap <$> argsEmbedded
      argsZipped <- zipWithErrorM pt.ptv xtargs' (ErrXtorArity loc sig.sigName)
      currVars <- getCheckerVars 
      let newVars = foldr (\(Tuple v ty') m -> insert v ty' m)  currVars argsZipped
      c' <- withCheckerVars newVars (checkCommand pt.ptcmd)
      pure $ T.Pattern {ptxt:pt.ptxt, ptv:pt.ptv, ptcmd:c'}

checkTerm (D.ShiftCBV loc t) (T.TyShift ty) = do 
  t' <- checkTerm t ty 
  let newTy = T.TyShift (T.getType t') 
  pure $ T.ShiftCBV loc t' newTy 

checkTerm (D.ShiftCBN loc t) (T.TyShift ty) = do 
  t' <- checkTerm t ty 
  let newTy = T.TyShift (T.getType t') 
  pure $ T.ShiftCBN loc t' newTy
  
checkTerm t ty = throwError (ErrBadType (getLoc t) t ty)

------------------------------------------------------------------------------------
------------------------------------- Commands -------------------------------------
------------------------------------------------------------------------------------

checkCommand :: D.Command -> CheckM T.Command 
checkCommand c@(D.Cut loc (D.Var loc1 v1) pol (D.Var loc2 v2)) = do
  mty1 <- getMTypeVar v1 
  mty2 <- getMTypeVar v2 
  case (Tuple mty1 mty2) of
    (Tuple Nothing Nothing)  -> throwError (ErrUnclearType loc c)
    (Tuple (Just ty1) Nothing) -> pure $ T.Cut loc (T.Var loc1 v1 ty1) pol (T.Var loc2 v2 ty1)
    (Tuple Nothing (Just ty2)) -> pure $ T.Cut loc (T.Var loc1 v1 ty2) pol (T.Var loc2 v2 ty2)
    (Tuple (Just ty1) (Just ty2)) -> if ty1 == ty2 then pure $ T.Cut loc (T.Var loc1 v1 ty1) pol (T.Var loc2 v2 ty2) else throwError (ErrUnclearType loc c)
checkCommand (D.Cut loc (D.Var loc1 v1) eo u) = do
  ty <- getTypeVar loc1 v1 
  u' <- checkTerm u ty
  pure $ T.Cut loc (T.Var loc1 v1 ty) eo u' 
checkCommand (D.Cut loc t eo (D.Var loc2 v2)) = do
  ty <- getTypeVar loc2 v2 
  t' <- checkTerm t ty
  pure $ T.Cut loc t' eo (T.Var loc2 v2 ty) 
checkCommand c@(D.Cut _ _ _ _) = throwError (ErrUnclearType (getLoc c) c)
checkCommand (D.CutAnnot loc t ty eo u) = do
  ty1 <- checkType loc ty
  ty2 <- checkType loc ty 
  t' <- checkTerm t ty1
  u' <- checkTerm u  ty2
  pure $ T.Cut loc t' eo u'

checkCommand (D.Done loc) = pure $ T.Done loc
checkCommand (D.Err loc err) = pure $ T.Err loc err
checkCommand (D.Print loc (D.Var loc1 v)) = do
  ty <- getTypeVar loc1 v 
  pure $ T.Print loc (T.Var loc1 v ty)
checkCommand c@(D.Print _ _) = throwError (ErrUnclearType (getLoc c) c)
checkCommand (D.PrintAnnot loc t ty) = do
  ty' <- checkType loc ty 
  t' <- checkTerm t ty'
  pure $ T.Print loc t'
