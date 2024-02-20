module TypeInference.GenerateConstraints where 

import Typed.Types 
import Typed.Program
import Untyped.Syntax qualified as S
import Typed.Syntax qualified as T
import TypeInference.Definition
import Common 

import Control.Monad.Except
import Control.Monad


checkPts :: [T.Pattern] -> GenM (Maybe DataDecl)
checkPts [] = return Nothing 
checkPts (pt:pts) = do 
  decl <- findDataDecl (T.ptxt pt)
  case decl of 
    Nothing -> throwError ("Xtor " <> show (T.ptxt pt) <> " used but not defined") 
    Just (d@(MkDataDecl _ _ _ xtors),_) -> if all ((`elem` (sigName <$> xtors)) . T.ptxt) pts then return (Just d) else return Nothing

runGenCmd :: Program -> S.Command -> Either String (T.Command,[Constraint])
runGenCmd prog cmd = runGenM prog (genConstraintsCmd cmd)
runGenT :: Program -> S.Term -> Either String (T.Term,[Constraint])
runGenT prog t = runGenM prog (genConstraintsTerm t)

genConstraintsCmd :: S.Command -> GenM T.Command 
genConstraintsCmd (S.Cut t pol u) = do 
  t' <- genConstraintsTerm t
  u' <- genConstraintsTerm u
  addConstraint (MkTyEq (T.getType t') (T.getType u'))
  pol' <- genConstraintsType (T.getType t')
  addConstraint (MkFlipEq (T.getKind t') (T.getKind u'))
  addConstraint (MkProdEq pol' (T.getKind t') (MkKind pol))
  return (T.Cut t' pol u')
genConstraintsCmd S.Done = return T.Done
  
genConstraintsTerm :: S.Term -> GenM T.Term 
genConstraintsTerm (S.Var v) = do 
   tyV <- freshTyVar 
   kndV <- freshKndVar
   let newT = TyVar tyV (MkKindVar kndV)
   addVar v newT 
   addTyVar tyV (MkKindVar kndV)
   return (T.Var v newT) 
genConstraintsTerm (S.Mu v c) = do 
  tyV <- freshTyVar
  kndV1 <- freshKndVar 
  kndV2 <- freshKndVar 
  addConstraint (MkFlipEq (MkKindVar kndV1) (MkKindVar kndV2))
  addVar v (TyVar tyV (MkKindVar kndV1))
  addTyVar tyV (MkKindVar kndV1)
  c' <- genConstraintsCmd c
  return $ T.Mu v c' (TyVar tyV (MkKindVar kndV2))

-- TODO generate new variables for the variables in the data declaration
-- otherwise we can only have the same type arguments for each time a declaration is used
genConstraintsTerm (S.Xtor nm args) = do 
  decl <- findDataDecl nm
  case decl of
    Nothing -> throwError ("Xtor " <> show nm <> " was used but not defined")
    Just (MkDataDecl{declNm=tyn, declArgs=_, declPol=pl, declSig=_},xtSig) -> do
      args' <- forM args genConstraintsTerm
      let argTys = T.getType <$> args'
      addConstraintsXtor nm argTys (sigArgs xtSig)
      let newT = TyDecl tyn argTys (MkKind pl)
      return (T.Xtor nm args' newT)
genConstraintsTerm (S.XCase pts)  = do 
  pts' <- mapM genConstraintsPt pts
  decl <- checkPts pts' 
  case decl of 
    Nothing -> throwError "Pattern not well-formed"
    Just MkDataDecl{declNm=tyn, declArgs=tyArgs, declPol=pl,declSig=_} -> do 
      forM_ pts (\pt -> do 
        forM_ (zip (S.ptv pt) tyArgs) (\(x,(y,z)) -> addVar x (TyVar y (MkKind z)))
        genConstraintsCmd (S.ptcmd pt))
      let newT = TyDecl tyn ( (\(v,p) -> TyVar v (MkKind p)) <$> tyArgs ) (MkKind pl)
      return (T.XCase pts' newT)
genConstraintsTerm (S.Shift t) = do 
  t' <- genConstraintsTerm t 
  addConstraint (MkKindEq (T.getKind t') (MkKind Pos))
  let newT = TyShift (T.getType t') (MkKind Pos)
  return (T.Shift t' newT)
genConstraintsTerm (S.Lam v cmd) = do  
  tyV <- freshTyVar 
  addVar v (TyVar tyV (MkKind Pos))
  cmd' <- genConstraintsCmd cmd
  let newT = TyShift (TyVar tyV (MkKind Pos)) (MkKind Neg)
  return (T.Lam v cmd' newT)
genConstraintsPt :: S.Pattern -> GenM T.Pattern
genConstraintsPt (S.MkPattern xt vs cmd) = do
  cmd' <- genConstraintsCmd cmd
  return $ T.MkPattern xt vs cmd'

genConstraintsType :: Ty -> GenM Pol 
genConstraintsType _ = return Pos
