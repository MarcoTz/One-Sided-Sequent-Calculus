module TypeInference.GenerateConstraints where 

import Typed.Types 
import Typed.Program
import Untyped.Syntax qualified as S
import Typed.Syntax qualified as T
import TypeInference.Definition
import Common 

import Control.Monad.Except
import Control.Monad


addConstraintsXtor :: [Ty] -> [Ty] -> GenM () 
addConstraintsXtor [] [] = return () 
addConstraintsXtor _ [] = throwError "Wrong number of arguments"
addConstraintsXtor [] _ = throwError "Wrong number of arguments"
addConstraintsXtor (ty1:tys1) (ty2:tys2) = do 
  addConstraint (MkTyEq ty1 ty2)
  addConstraint (MkKindEq (T.getKind ty1) (T.getKind ty2))
  addConstraintsXtor tys1 tys2

checkPts :: [T.Pattern] -> GenM (Maybe Decl)
checkPts [] = return Nothing 
checkPts (pt:pts) = do 
  decl <- findDataDecl (T.ptxt pt)
  case decl of 
    Nothing -> throwError ("Xtor " <> show (T.ptxt pt) <> " used but not defined") 
    Just (MkDataDecl _ _ _ xtors) -> if all ((`elem` (sigName <$> xtors)) . T.ptxt) pts then return decl else return Nothing
    Just _ -> error "expeceted data declaration but found different declaration (should never happen)"


genConstraintsCmd :: S.Command -> GenM T.Command 
genConstraintsCmd (S.Cut t pol u) = do 
  t' <- genConstraintsTerm t
  u' <- genConstraintsTerm u
  addConstraint (MkTyEq (T.getType t') (T.getType u'))
  pol' <- genConstraintsType (T.getType t')
  addConstraint (MkFlipEq (T.getKind t') (T.getKind u'))
  addConstraint (MkProdEq pol' (T.getKind t') (MkKind pol))
  return (T.Cut t' pol u')
  
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
    Just MkDataDecl{declNm=tyn, declArgs=tyArgs, declPol=pl, declSig=xtors} -> 
      case findXtor nm xtors of 
        Nothing -> error "Xtor could not be found in declaration (should never happen)"
        Just _ -> do
          args' <- forM args genConstraintsTerm
          let argTys = T.getType <$> args'
          let declTys = (\(v,p) -> TyVar v (MkKind p)) <$> tyArgs
          addConstraintsXtor argTys declTys 
          let newT = TyDecl tyn argTys (MkKind pl)
          return (T.Xtor nm args' newT)
    Just _ -> error "expected DataDecl but found different one (should never happen)"
  where 
    findXtor :: XtorName -> [XtorSig] -> Maybe XtorSig
    findXtor _ [] = Nothing 
    findXtor n (sig:sigs) = if n == sigName sig then Just sig else findXtor n sigs
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
    Just _ -> error "expected data declaration but got other declaration (should never happen)" 
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
