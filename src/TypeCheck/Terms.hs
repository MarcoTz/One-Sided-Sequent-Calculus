module TypeCheck.Terms where 

import TypeCheck.Definition
import TypeCheck.Types
import Syntax.Typed.Terms     qualified as T
import Syntax.Desugared.Terms qualified as D
import Syntax.Desugared.Types qualified as D
import Errors
import Common

import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import Data.Map qualified as M


-- WIP, should not be working yet 
checkTerm :: D.Term -> D.TypeScheme -> CheckM T.Term
checkTerm (D.Var v) (D.MkTypeScheme _tyvars ty) = do
  vars <- gets checkVars 
  ty' <- checkType ty Pos
  case M.lookup v vars of 
    Nothing -> throwError (ErrMissingVar v WhereCheck)
    Just ty'' -> if ty'' == ty' then return (T.Var v ty'') else throwError (ErrTypeNeq ty'' ty' WhereCheck)
checkTerm (D.Mu v c) (D.MkTypeScheme _tyvars ty) = do
  ty' <- checkType ty Pos
  addVar v ty' 
  c' <- checkCommand c
  return (T.Mu v c' ty')

checkTerm (D.Xtor nm args) tys@(D.MkTypeScheme _tyargs ty) = do 
  args' <- forM args (`checkTerm` tys)
  ty' <- checkType ty Pos
  return (T.Xtor nm args' ty')

checkTerm (D.XCase pts) (D.MkTypeScheme _tyargs ty) = do
  ty' <- checkType ty Pos 
  pts' <- checkPatterns pts 
  return (T.XCase pts' ty')
  where 
    checkPatterns :: [D.Pattern] -> CheckM [T.Pattern]
    checkPatterns [] = return []
    checkPatterns (D.MkPattern xt vars c:pts') = do 
      c' <- checkCommand c
      let newPt = T.MkPattern xt vars c'
      newPts <- checkPatterns pts'
      return (newPt:newPts)

checkTerm (D.Shift t) d@(D.MkTypeScheme _tyargs ty) = do
  t' <- checkTerm t d
  ty' <- checkType ty Pos
  let knd = getKind ty'
  if knd /= Pos then throwError (ErrKind (MkKind knd) (MkKind Pos) ShouldEq WhereCheck) else return (T.Shift t' ty')

checkTerm (D.Lam v c) (D.MkTypeScheme _tyargs ty) = do
  c' <- checkCommand c 
  ty' <- checkType ty Pos
  return (T.Lam v c' ty') 

checkCommand :: D.Command -> CheckM T.Command
checkCommand (D.Cut t pol u)  = do
  t' <- checkTerm t (D.MkTypeScheme [] (D.TyVar $ MkTypeVar "nothing"))
  u' <- checkTerm u (D.MkTypeScheme [] (D.TyVar $ MkTypeVar "nothing"))
  return $ T.Cut t' pol u'
checkCommand D.Done = return T.Done 
