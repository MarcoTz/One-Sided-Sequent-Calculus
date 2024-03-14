module TypeInference.GenerateConstraints.Terms where 


import Syntax.Typed.Types 
import Syntax.Typed.Program
import Syntax.Desugared.Terms qualified as D
import Syntax.Typed.Terms qualified as T
import Syntax.Typed.Substitution qualified as T
import TypeInference.GenerateConstraints.Definition
import TypeInference.Constraints
import Errors
import Common
import Environment

import Control.Monad.Except
import Control.Monad.State
import Control.Monad
import Data.Map qualified as M


checkPts :: [D.Pattern] -> GenM (Maybe DataDecl)
checkPts [] = return Nothing 
checkPts (pt:pts) = do 
  d@(MkData _ _ _ xtors) <- lookupXtorDecl (D.ptxt pt)
  if all ((`elem` (sigName <$> xtors)) . D.ptxt) pts then return (Just d) else return Nothing

genConstraintsCmd :: D.Command -> GenM T.Command 
genConstraintsCmd (D.Cut t pol u) = do 
  t' <- genConstraintsTerm t
  u' <- genConstraintsTerm u
  let pol1 = getKind t' 
  let pol2 = getKind u'
  if pol1 == pol2 then throwError (ErrKind ShouldEq (T.getType t') (T.getType u') "genConstraintsCmd") else do 
    addConstraint (MkTyEq (T.getType t') (T.getType u'))
    return (T.Cut t' pol u')
genConstraintsCmd (D.CutAnnot t _ pol u) = genConstraintsCmd (D.Cut t pol u)
genConstraintsCmd D.Done = return T.Done
genConstraintsCmd (D.Err err) = return $ T.Err err
  
genConstraintsTerm :: D.Term -> GenM T.Term 
genConstraintsTerm (D.Var v) = do 
   vars <- gets varEnv
   case M.lookup v vars of 
     Nothing -> do 
       tyV <- freshTyVar Pos 
       addVar v tyV
       return (T.Var v tyV) 
     Just ty -> return (T.Var v ty)

genConstraintsTerm (D.Mu v  c) = do 
  tyV <- freshTyVar Pos 
  addVar v tyV
  c' <- genConstraintsCmd c
  return $ T.Mu v c' tyV 

genConstraintsTerm (D.Xtor nm args) = do 
  (MkData tyn tyargs _ _) <- lookupXtorDecl nm
  xtSig <- lookupXtor nm
  (newVars,varmap) <- freshTyVarsDecl tyargs
  args' <- forM args genConstraintsTerm
  let argTys = T.getType <$> args'
  let varsSubst = T.substTyVars varmap <$>  sigArgs xtSig
  addConstraintsXtor nm argTys varsSubst
  let newT = TyDecl tyn newVars Pos
  return (T.Xtor nm args' newT)
genConstraintsTerm (D.XCase pts)  = do 
  decl <- checkPts pts
  case decl of 
    Nothing -> throwError (ErrBadPattern (D.ptxt <$> pts) "genConstraintsTerm (XCase)")
    Just (MkData tyn tyArgs _ _) -> do
      (newVars, varmap) <- freshTyVarsDecl tyArgs
      pts' <- forM pts (\pt -> do 
        forM_ (zip (D.ptv pt) newVars) (uncurry addVar) 
        c' <- genConstraintsCmd (D.ptcmd pt)
        return $ T.MkPattern (D.ptxt pt) (D.ptv pt) c' )
      let pts'' = T.substTyVars varmap <$> pts'
      let newT = TyDecl tyn newVars Pos
      return (T.XCase pts'' newT)
genConstraintsTerm (D.ShiftPos t) = do 
  t' <- genConstraintsTerm t 
  let pol = getKind t' 
  if pol /= Pos then throwError (ErrKind  ShouldEq (T.getType t') (TyShift (T.getType t')) "genConstraintsTerm (Shift)") else do 
    let newT = TyShift (T.getType t')
    return (T.ShiftPos t' newT)
genConstraintsTerm (D.ShiftNeg v cmd) = do  
  tyV <- freshTyVar Pos 
  addVar v tyV
  cmd' <- genConstraintsCmd cmd
  let newT = TyShift tyV 
  return (T.ShiftNeg v cmd' newT)
