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
  addConstraint (MkTyEq (T.getType t') (T.getType u'))
  return (T.Cut loc t' eo u')
genConstraintsCmd (D.CutAnnot loc t ty eo u) = do
  t' <- genConstraintsTerm t
  let ty1' = T.getType t'
  u' <- genConstraintsTerm u 
  ty' <- genConstraintsTy loc ty 
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
  ty' <- genConstraintsTy loc ty
  addConstraint (MkTyEq (T.getType t') ty')
  return $ T.Print loc t'
genConstraintsTerm :: D.Term -> GenM T.Term 
genConstraintsTerm (D.Var loc v) = do 
   vars <- getGenVars 
   case M.lookup v vars of 
     Nothing -> do 
       tyV <- freshTyVar
       addGenVar v tyV
       return (T.Var loc v tyV) 
     Just ty -> return (T.Var loc v ty)

genConstraintsTerm (D.Mu loc v  c) = do 
  tyV <- freshTyVar
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
  let newT = TyDecl tyn newVars 
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
      let newT = TyDecl tyn newVars 
      return (T.XCase loc pts'' newT)
genConstraintsTerm (D.ShiftCBV loc t) = do 
  t' <- genConstraintsTerm t 
  let newT = TyShift (T.getType t') 
  return (T.ShiftCBV loc t' newT)
genConstraintsTerm (D.ShiftCBN loc t) = do  
  t' <- genConstraintsTerm t 
  let newT = TyShift (T.getType t') 
  return (T.ShiftCBN loc t' newT)
