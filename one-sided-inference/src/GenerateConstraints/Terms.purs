module GenerateConstraints.Terms (
  genConstraintsTerm,
  genConstraintsCmd
) where 

import GenerateConstraints.Definition (GenM, addConstraint,getGenVars,freshTyVar, addGenVar, freshTyVarsDecl, addConstraintsXtor)
import GenerateConstraints.Errors (GenerateError(..))
import GenerateConstraints.Types (genConstraintsTy)
import Constraints (Constr(..))
import Loc (Loc) 
import Environment (lookupXtorDecl, lookupXtor)
import Syntax.Desugared.Terms (Term(..), Pattern(..),Command(..)) as D
import Syntax.Typed.Terms (Command(..),getType,Term(..),Pattern(..)) as T
import Syntax.Typed.Substitution (substTyvars) as T
import Syntax.Typed.Types (Ty(..)) as T
import Syntax.Kinded.Program (DataDecl(..),XtorSig(..)) as K
import Syntax.Kinded.Types (embedType) 

import Prelude (bind,pure, (<$>),($),not)
import Data.List (List(..),elem,null,filter, zip)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Map (lookup)
import Data.Traversable (for)
import Control.Monad.Except (throwError)

checkPts :: Loc -> List D.Pattern -> GenM (Maybe K.DataDecl)
checkPts _ Nil = pure Nothing 
checkPts loc (Cons (D.Pattern pt) pts) = do 
  (K.DataDecl decl) <- lookupXtorDecl loc pt.ptxt
  let sigNames = (\(K.XtorSig sig) -> sig.sigName) <$> decl.declXtors
  let ptNames = (\(D.Pattern pt') -> pt'.ptxt) <$> pts
  let notElems = filter (\xtn -> not $ xtn `elem` sigNames) ptNames
  if null notElems then pure (Just (K.DataDecl decl)) else pure Nothing

genConstraintsCmd :: D.Command -> GenM T.Command 
genConstraintsCmd (D.Cut loc t eo u) = do 
  t' <- genConstraintsTerm t
  u' <- genConstraintsTerm u
  _ <- addConstraint (MkTyEq (T.getType t') (T.getType u'))
  pure (T.Cut loc t' eo u')
genConstraintsCmd (D.CutAnnot loc t ty eo u) = do
  t' <- genConstraintsTerm t
  let ty1' = T.getType t'
  u' <- genConstraintsTerm u 
  ty' <- genConstraintsTy loc ty 
  _ <- addConstraint (MkTyEq ty1' (T.getType u'))
  _ <- addConstraint (MkTyEq ty1' ty')
  pure $ T.Cut loc t' eo u'
genConstraintsCmd (D.Done loc) = pure $ T.Done loc
genConstraintsCmd (D.Err loc err) = pure $ T.Err loc err
genConstraintsCmd (D.Print loc t) = do 
  t' <- genConstraintsTerm t
  pure $ T.Print loc t'  
genConstraintsCmd (D.PrintAnnot loc t ty) = do
  t' <- genConstraintsTerm t
  ty' <- genConstraintsTy loc ty
  _ <- addConstraint (MkTyEq (T.getType t') ty')
  pure $ T.Print loc t'
genConstraintsTerm :: D.Term -> GenM T.Term 
genConstraintsTerm (D.Var loc v) = do 
   vars <- getGenVars 
   case lookup v vars of 
     Nothing -> do 
       tyV <- freshTyVar
       _ <- addGenVar v tyV
       pure (T.Var loc v tyV) 
     Just ty -> pure (T.Var loc v ty)

genConstraintsTerm (D.Mu loc v  c) = do 
  tyV <- freshTyVar
  _ <- addGenVar v tyV
  c' <- genConstraintsCmd c
  pure $ T.Mu loc v c' tyV 

genConstraintsTerm (D.Xtor loc nm args) = do 
  (K.DataDecl decl) <- lookupXtorDecl loc nm
  (K.XtorSig sig) <- lookupXtor loc nm
  (Tuple newVars varmap) <- freshTyVarsDecl decl.declArgs 
  args' <- for args genConstraintsTerm
  let argTys = T.getType <$> args'
  let varsSubst = (\ty -> T.substTyvars varmap (embedType ty)) <$> sig.sigArgs
  _ <- addConstraintsXtor loc nm argTys varsSubst
  let newT = T.TyDecl decl.declName newVars 
  pure (T.Xtor loc nm args' newT)
genConstraintsTerm (D.XCase loc pts)  = do 
  mdecl <- checkPts loc pts
  case mdecl of 
    Nothing -> throwError (ErrBadPattern loc ((\(D.Pattern pt) -> pt.ptxt) <$> pts))
    Just (K.DataDecl decl) -> do
      (Tuple newVars varmap) <- freshTyVarsDecl decl.declArgs 
      pts' <- for pts (\(D.Pattern pt) -> do 
        _ <- for (zip pt.ptv newVars) (\(Tuple x y) -> addGenVar x y) 
        c' <- genConstraintsCmd pt.ptcmd
        pure $ T.Pattern {ptxt:pt.ptxt,ptv:pt.ptv,ptcmd:c'})
      let pts'' = T.substTyvars varmap <$> pts'
      let newT = T.TyDecl decl.declName newVars 
      pure (T.XCase loc pts'' newT)
genConstraintsTerm (D.ShiftCBV loc t) = do 
  t' <- genConstraintsTerm t 
  let newT = T.TyShift (T.getType t') 
  pure (T.ShiftCBV loc t' newT)
genConstraintsTerm (D.ShiftCBN loc t) = do  
  t' <- genConstraintsTerm t 
  let newT = T.TyShift (T.getType t') 
  pure (T.ShiftCBN loc t' newT)
