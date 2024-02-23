module TypeInference.GenerateConstraints.Terms where 


import Syntax.Typed.Types 
import Syntax.Typed.Program
import Syntax.Desugared.Terms qualified as D
import Syntax.Typed.Terms qualified as T
import TypeInference.GenerateConstraints.Definition
import TypeInference.Constraints
import Common 
import Errors

import Control.Monad.Except
import Control.Monad

checkPts :: [D.Pattern] -> GenM (Maybe DataDecl)
checkPts [] = return Nothing 
checkPts (pt:pts) = do 
  decl <- findDataDecl (D.ptxt pt)
  case decl of 
    Nothing -> throwError (ErrXtorUndefined (D.ptxt pt))
    Just (d@(MkDataDecl _ _ _ xtors),_) -> if all ((`elem` (sigName <$> xtors)) . D.ptxt) pts then return (Just d) else return Nothing

genConstraintsCmd :: D.Command -> GenM T.Command 
genConstraintsCmd (D.Cut t pol u) = do 
  t' <- genConstraintsTerm t
  u' <- genConstraintsTerm u
  insertConstraint (MkTyEq (T.getType t') (T.getType u'))
  pol' <- genConstraintsType (T.getType t')
  insertConstraint (MkFlipEq (T.getKind t') (T.getKind u'))
  let newConstr = case pol' of Pos -> MkKindEq; Neg -> MkFlipEq 
  insertConstraint (newConstr (T.getKind t') (MkKind pol))
  return (T.Cut t' pol u')
genConstraintsCmd D.Done = return T.Done
  
genConstraintsTerm :: D.Term -> GenM T.Term 
genConstraintsTerm (D.Var v) = do 
   tyV <- freshTyVar 
   kndV <- freshKndVar
   let newT = TyVar tyV (MkKindVar kndV)
   addVar v newT 
   addTyVar tyV (MkKindVar kndV)
   return (T.Var v newT) 
genConstraintsTerm (D.Mu v c) = do 
  tyV <- freshTyVar
  kndV1 <- freshKndVar 
  kndV2 <- freshKndVar 
  insertConstraint (MkFlipEq (MkKindVar kndV1) (MkKindVar kndV2))
  addVar v (TyVar tyV (MkKindVar kndV1))
  addTyVar tyV (MkKindVar kndV1)
  c' <- genConstraintsCmd c
  return $ T.Mu v c' (TyVar tyV (MkKindVar kndV2))

-- TODO generate new variables for the variables in the data declaration
-- otherwise we can only have the same type arguments for each time a declaration is used
genConstraintsTerm (D.Xtor nm args) = do 
  decl <- findDataDecl nm
  case decl of
    Nothing -> throwError (ErrXtorUndefined nm) 
    Just (MkDataDecl{declNm=tyn, declArgs=tyargs, declPol=pl, declSig=_},xtSig) -> do
      (newVars,varmap) <- freshTyVarsDecl tyargs
      args' <- forM args genConstraintsTerm
      let argTys = T.getType <$> args'
      let varsSubst = substVars varmap <$>  sigArgs xtSig
      let newTyArgs = (\(v,p) -> TyVar v (MkKind p)) <$> newVars
      addConstraintsXtor nm argTys varsSubst
      let newT = TyDecl tyn newTyArgs (MkKind pl)
      return (T.Xtor nm args' newT)
genConstraintsTerm (D.XCase pts)  = do 
  decl <- checkPts pts
  case decl of 
    Nothing -> throwError (ErrPatMalformed (D.ptxt <$> pts))
    Just MkDataDecl{declNm=tyn, declArgs=tyArgs, declPol=pl,declSig=_} -> do 
      (newVars, varmap) <- freshTyVarsDecl tyArgs
      pts' <- forM pts (\pt -> do 
        forM_ (zip (D.ptv pt) newVars) (\(x,(y,z)) -> addVar x (TyVar y (MkKind z)))
        c' <- genConstraintsCmd (D.ptcmd pt)
        return $ T.MkPattern (D.ptxt pt) (D.ptv pt) c' )
      let pts'' = substVars varmap <$> pts'
      let newTyArgs = (\(v,p) -> TyVar v (MkKind p)) <$> newVars
      let newT = TyDecl tyn newTyArgs (MkKind pl)
      return (T.XCase pts'' newT)
genConstraintsTerm (D.Shift t) = do 
  t' <- genConstraintsTerm t 
  insertConstraint (MkKindEq (T.getKind t') (MkKind Pos))
  let newT = TyShift (T.getType t') (MkKind Pos)
  return (T.Shift t' newT)
genConstraintsTerm (D.Lam v cmd) = do  
  tyV <- freshTyVar 
  addVar v (TyVar tyV (MkKind Pos))
  cmd' <- genConstraintsCmd cmd
  let newT = TyShift (TyVar tyV (MkKind Pos)) (MkKind Neg)
  return (T.Lam v cmd' newT)

genConstraintsType :: Ty -> GenM Pol 
genConstraintsType _ = return Pos
