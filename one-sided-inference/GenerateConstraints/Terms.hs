module GenerateConstraints.Terms (
  genConstraintsTerm
) where 

import Syntax.Typed.Types 
import Syntax.Typed.Program
import Syntax.Desugared.Terms qualified as D
import Syntax.Typed.Terms qualified as T
import Syntax.Typed.Substitution qualified as T
import GenerateConstraints.Definition
import GenerateConstraints.Types
import Constraints
import Common
import Loc
import Environment
import Embed.EmbedTyped ()

import Control.Monad.Except
import Control.Monad
import Data.Map qualified as M


checkPts :: Loc -> [D.Pattern] -> GenM (Maybe DataDecl)
checkPts _ [] = return Nothing 
checkPts loc (pt:pts) = do 
  d@(MkData _ _ _ _ xtors) <- lookupXtorDecl loc (D.ptxt pt)
  if all ((`elem` (sigName <$> xtors)) . D.ptxt) pts then return (Just d) else return Nothing

genConstraintsCmd :: D.Command -> GenM T.Command 
genConstraintsCmd (D.Cut loc t eo u) = do 
  t' <- genConstraintsTerm t
  u' <- genConstraintsTerm u
  let eo1 = getKind t' 
  let eo2 = getKind u'
  if eo1 == eo2 then throwError (ErrKindNeq loc (T.getType t') (T.getType u')) else do 
    addConstraint (MkTyEq (T.getType t') (T.getType u'))
    return (T.Cut loc t' eo u')
genConstraintsCmd (D.CutAnnot loc t ty eo u) = do
  t' <- genConstraintsTerm t
  let ty1' = T.getType t'
  u' <- genConstraintsTerm u 
  kndv <- freshKVar
  ty' <- genConstraintsTy loc ty kndv
  let eo1 = getKind t' 
  let eo2 = getKind u' 
  let eo3 = getKind ty'
  if eo1 /= eo3 || eo1 == eo2 then throwError (ErrKindNeq loc ty1' (T.getType u')) else do
    addConstraint (MkTyEq ty1' (T.getType u'))
    addConstraint (MkTyEq ty1' ty')
    return $ T.Cut loc t' eo u'
genConstraintsCmd (D.Done loc) = return $ T.Done loc
genConstraintsCmd (D.Err loc err) = return $ T.Err loc err
genConstraintsCmd (D.Print loc t) = do 
  t' <- genConstraintsTerm t
  return $ T.Print loc t'  
genConstraintsCmd (D.PrintAnnot loc t ty) = do
  t' <- genConstraintsTerm t
  kndv <- freshKVar 
  ty' <- genConstraintsTy loc ty kndv
  addConstraint (MkTyEq (T.getType t') ty')
  return $ T.Print loc t'
genConstraintsTerm :: D.Term -> GenM T.Term 
genConstraintsTerm (D.Var loc v) = do 
   vars <- getGenVars 
   case M.lookup v vars of 
     Nothing -> do 
       tyV <- freshTyVar CBV
       addGenVar v tyV
       return (T.Var loc v tyV) 
     Just ty -> return (T.Var loc v ty)

genConstraintsTerm (D.Mu loc v  c) = do 
  tyV <- freshTyVar CBV
  addGenVar v tyV
  c' <- genConstraintsCmd c
  return $ T.Mu loc v c' tyV 

genConstraintsTerm (D.Xtor loc nm args) = do 
  (MkData _ tyn tyargs isco _) <- lookupXtorDecl loc nm
  xtSig <- lookupXtor loc nm
  (newVars,varmap) <- freshTyVarsDecl tyargs (defaultEo isco)
  args' <- forM args genConstraintsTerm
  let argTys = T.getType <$> args'
  let varsSubst = T.substTyVars varmap <$>  sigArgs xtSig
  addConstraintsXtor loc nm argTys varsSubst
  let newT = TyDecl tyn newVars (MkKind CBV)
  return (T.Xtor loc nm args' newT)
genConstraintsTerm (D.XCase loc pts)  = do 
  decl <- checkPts loc pts
  case decl of 
    Nothing -> throwError (ErrBadPattern loc (D.ptxt <$> pts))
    Just (MkData _ tyn tyArgs isco _) -> do
      (newVars, varmap) <- freshTyVarsDecl tyArgs (defaultEo isco)
      pts' <- forM pts (\pt -> do 
        forM_ (zip (D.ptv pt) newVars) (uncurry addGenVar) 
        c' <- genConstraintsCmd (D.ptcmd pt)
        return $ T.MkPattern (D.ptxt pt) (D.ptv pt) c' )
      let pts'' = T.substTyVars varmap <$> pts'
      let newT = TyDecl tyn newVars (MkKind CBV)
      return (T.XCase loc pts'' newT)
genConstraintsTerm (D.ShiftCBV loc t) = do 
  t' <- genConstraintsTerm t 
  addConstraint (MkKindEq (getKind t') (MkKind CBV))
  let newT = TyShift (T.getType t') (MkKind CBV)
  return (T.ShiftCBV loc t' newT)
genConstraintsTerm (D.ShiftCBN loc t) = do  
  t' <- genConstraintsTerm t 
  addConstraint (MkKindEq (getKind t') (MkKind CBV))
  let newT = TyShift (T.getType t') (MkKind CBN)
  return (T.ShiftCBN loc t' newT)
