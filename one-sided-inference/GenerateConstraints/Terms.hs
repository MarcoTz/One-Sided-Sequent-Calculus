module GenerateConstraints.Terms (
  genConstraintsTerm
) where 

import Syntax.Typed.Types 
import Syntax.Typed.Program
import Syntax.Desugared.Terms qualified as D
import Syntax.Typed.Terms qualified as T
import Syntax.Typed.Substitution qualified as T
import GenerateConstraints.Definition
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
genConstraintsCmd (D.Cut loc t pol u) = do 
  t' <- genConstraintsTerm t
  u' <- genConstraintsTerm u
  let pol1 = getKind t' 
  let pol2 = getKind u'
  if pol1 == pol2 then throwError (ErrKindNeq loc (T.getType t') (T.getType u')) else do 
    addConstraint (MkTyEq (T.getType t') (T.getType u'))
    return (T.Cut loc t' pol u')
genConstraintsCmd (D.CutAnnot loc t _ pol u) = genConstraintsCmd (D.Cut loc t pol u)
genConstraintsCmd (D.Done loc) = return $ T.Done loc
genConstraintsCmd (D.Err loc err) = return $ T.Err loc err
  
genConstraintsTerm :: D.Term -> GenM T.Term 
genConstraintsTerm (D.Var loc v) = do 
   vars <- getGenVars 
   case M.lookup v vars of 
     Nothing -> do 
       tyV <- freshTyVar Pos 
       addGenVar v tyV
       return (T.Var loc v tyV) 
     Just ty -> return (T.Var loc v ty)

genConstraintsTerm (D.Mu loc v  c) = do 
  tyV <- freshTyVar Pos 
  addGenVar v tyV
  c' <- genConstraintsCmd c
  return $ T.Mu loc v c' tyV 

genConstraintsTerm (D.Xtor loc nm args) = do 
  (MkData _ tyn tyargs _ _) <- lookupXtorDecl loc nm
  xtSig <- lookupXtor loc nm
  (newVars,varmap) <- freshTyVarsDecl tyargs
  args' <- forM args genConstraintsTerm
  let argTys = T.getType <$> args'
  let varsSubst = T.substTyVars varmap <$>  sigArgs xtSig
  addConstraintsXtor loc nm argTys varsSubst
  let newT = TyDecl tyn newVars Pos
  return (T.Xtor loc nm args' newT)
genConstraintsTerm (D.XCase loc pts)  = do 
  decl <- checkPts loc pts
  case decl of 
    Nothing -> throwError (ErrBadPattern loc (D.ptxt <$> pts))
    Just (MkData _ tyn tyArgs _ _) -> do
      (newVars, varmap) <- freshTyVarsDecl tyArgs
      pts' <- forM pts (\pt -> do 
        forM_ (zip (D.ptv pt) newVars) (uncurry addGenVar) 
        c' <- genConstraintsCmd (D.ptcmd pt)
        return $ T.MkPattern (D.ptxt pt) (D.ptv pt) c' )
      let pts'' = T.substTyVars varmap <$> pts'
      let newT = TyDecl tyn newVars Pos
      return (T.XCase loc pts'' newT)
genConstraintsTerm (D.ShiftPos loc t) = do 
  t' <- genConstraintsTerm t 
  let pol = getKind t' 
  if pol /= Pos then throwError (ErrKindNeq loc (T.getType t') (TyShift (T.getType t') Pos)) else do 
    let newT = TyShift (T.getType t') Pos
    return (T.ShiftPos loc t' newT)
genConstraintsTerm (D.ShiftNeg loc v cmd) = do  
  tyV <- freshTyVar Pos 
  addGenVar v tyV
  cmd' <- genConstraintsCmd cmd
  let newT = TyShift tyV Neg
  return (T.ShiftNeg loc v cmd' newT)
