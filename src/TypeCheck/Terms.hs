module TypeCheck.Terms where 

import TypeCheck.Definition
import Syntax.Typed.Terms
import Syntax.Typed.Types
import Errors
import Common
import Environment

import Control.Monad.Except
import Control.Monad.State
import Data.Map qualified as M

checkTerm :: Term -> CheckM () 
checkTerm (Var v ty) = do
  vars <- gets checkVars 
  case M.lookup v vars of 
    Nothing -> throwError (ErrVarUndefined v)
    Just ty' -> if ty == ty' then return () else throwError (ErrTyNeq ty ty')
checkTerm (Mu v c ty) = addVar v ty >> checkCommand c

checkTerm (Xtor nm args ty) = do 
  decl <- lookupXtor nm
  return () 
checkTerm (XCase pts ty) = return ()

checkTerm (Shift t ty) = do
  checkTerm t 
  let ty' = getType t 
  let knd = getKind ty 
  if knd /= Pos then throwError (ErrKindMisMatch Pos knd) else 
    if ty == ty' then return () else throwError (ErrTyNeq ty ty')

checkTerm (Lam v c ty) = do
  if getKind ty == Neg then do
    let ty' = flipPolTy ty
    addVar v ty'
    checkCommand c
  else throwError (ErrKindMisMatch Neg (getKind ty)) 

checkCommand :: Command -> CheckM ()
checkCommand (Cut t _ u) = 
  let pol1 = getKind (getType t)
      pol2 = getKind (getType u) in
  if pol1 == pol2 then throwError (ErrKindMisMatch (flipPol pol1) pol2) else do 
    checkTerm t 
    checkTerm u 
checkCommand Done = return ()
