module TypeInference.GenerateConstraints where 

import Typed.Types 
import Untyped.Syntax 
import TypeInference.Definition
import Common 

import Control.Monad.Except
import Control.Monad


addConstraintsXtor :: [(Ty,Kind)] -> [(Ty,Kind)] -> GenM () 
addConstraintsXtor [] [] = return () 
addConstraintsXtor _ [] = throwError "Wrong number of arguments"
addConstraintsXtor [] _ = throwError "Wrong number of arguments"
addConstraintsXtor ((ty1,knd1):tys1) ((ty2,knd2):tys2) = do 
  addConstraint (MkTyEq ty1 ty2)
  addConstraint (MkKindEq knd1 knd2)
  addConstraintsXtor tys1 tys2

checkPts :: [Pattern] -> GenM (Maybe Decl)
checkPts [] = return Nothing 
checkPts (pt:pts) = do 
  decl <- findDataDecl (ptxt pt)
  case decl of 
    Nothing -> throwError ("Xtor " <> show (ptxt pt) <> " used but not defined") 
    Just (MkDataDecl _ _ _ xtors) -> if all ((`elem` (sigName <$> xtors)) . ptxt) pts then return decl else return Nothing
    Just _ -> error "expeceted data declaration but found different declaration (should never happen)"


genConstraintsCmd :: Command -> GenM () 
genConstraintsCmd (Cut t pol u) = do 
  (ty1,pol1) <- genConstraintsTerm t
  (ty2,pol2) <- genConstraintsTerm u
  pol3 <- genConstraintsType ty1
  addConstraint (MkFlipEq pol2 pol1)
  addConstraint (MkProdEq pol3 pol1 (MkKind pol))
  addConstraint (MkTyEq ty1 ty2)
  
genConstraintsTerm :: Term -> GenM (Ty,Kind)
genConstraintsTerm (Var v) = do 
   tyV <- freshTyVar 
   kndV <- freshKndVar
   addVar v (TyVar tyV, MkKindVar kndV)
   addTyVar tyV (MkKindVar kndV)
   return (TyVar tyV, MkKindVar kndV)
genConstraintsTerm (Mu v c) = do 
  tyV <- freshTyVar
  kndV1 <- freshKndVar 
  kndV2 <- freshKndVar 
  addConstraint (MkFlipEq (MkKindVar kndV1) (MkKindVar kndV2))
  addVar v (TyVar tyV, MkKindVar kndV1)
  addTyVar tyV (MkKindVar kndV1)
  genConstraintsCmd c
  return (TyVar tyV, MkKindVar kndV2)

-- TODO generate new variables for the variables in the data declaration
-- otherwise we can only have the same type arguments for each time a declaration is used
genConstraintsTerm (Xtor nm args) = do 
  decl <- findDataDecl nm
  case decl of
    Nothing -> throwError ("Xtor " <> show nm <> " was used but not defined")
    Just MkDataDecl{declNm=tyn, declArgs=tyArgs, declPol=pl, declSig=xtors} -> 
      case findXtor nm xtors of 
        Nothing -> error "Xtor could not be found in declaration (should never happen)"
        Just xt -> do
          argTys <- forM args genConstraintsTerm
          addConstraintsXtor argTys (zip (sigArgs xt) (MkKind . snd <$> tyArgs))
          return (TyDecl tyn (TyVar . fst <$> tyArgs),MkKind pl)
    Just _ -> error "expected DataDecl but found different one (should never happen)"
  where 
    findXtor :: XtorName -> [XtorSig] -> Maybe XtorSig
    findXtor _ [] = Nothing 
    findXtor n (sig:sigs) = if n == sigName sig then Just sig else findXtor n sigs
genConstraintsTerm (XCase pts)  = do 
  decl <- checkPts pts 
  case decl of 
    Nothing -> throwError "Pattern not well-formed"
    Just MkDataDecl{declNm=tyn, declArgs=tyArgs, declPol=pl,declSig=_} -> do 
      forM_ pts (\pt -> do 
        forM_ (zip (ptv pt) tyArgs) (\(x,(y,z)) -> addVar x (TyVar y,MkKind z))
        genConstraintsCmd (ptcmd pt))
      return (TyDecl tyn (TyVar . fst <$> tyArgs), MkKind pl)
    Just _ -> error "expected data declaration but got other declaration (should never happen)" 
genConstraintsTerm (Shift t) = do 
  (ty,knd) <- genConstraintsTerm t 
  addConstraint (MkKindEq knd (MkKind Pos))
  return (TyShift ty, knd)
genConstraintsTerm (Lam v cmd) = do  
  tyV <- freshTyVar 
  addVar v (TyVar tyV, MkKind Pos)
  genConstraintsCmd cmd
  return (TyShift (TyVar tyV), MkKind Neg)

genConstraintsType :: Ty -> GenM Pol
genConstraintsType _ = return Pos
