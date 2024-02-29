module Desugar.Program where 

import Errors
import Environment
import Desugar.Definition
import Desugar.Terms
import Syntax.Parsed.Program qualified as P
import Syntax.Desugared.Program qualified as D
import Syntax.Desugared.Types qualified as D

import Control.Monad.Except
import Control.Monad 
import Data.Map qualified as M


checkNames :: Eq a => [a] -> (a -> Error) -> DesugarM () 
checkNames [] _ = return ()
checkNames (nm1:nms) err = if nm1 `elem` nms then throwError (err nm1) else checkNames nms err

desugarDecl :: P.DataDecl -> DesugarM D.DataDecl
desugarDecl d@(P.MkDataDecl tyn tyargs  pol sigs)= do 
  setCurrDecl d
  sigs' <- forM sigs desugarXtorSig
  return $ D.MkDataDecl tyn tyargs pol sigs'

desugarVar :: P.VarDecl -> DesugarM D.VarDecl
desugarVar (P.MkVarDecl v t) = do 
  t' <- desugarTerm t
  return $ D.MkVarDecl v t'

desugarXtorSig :: P.XtorSig -> DesugarM D.XtorSig
desugarXtorSig (P.MkXtorSig xtn args) = do
  args' <- forM args desugarTy
  return (D.MkXtorSig xtn args')

desugarTy :: P.Ty -> DesugarM D.Ty
-- a type variable appearing in  a declaration is either 
-- an actual variable that is the argument of the current declaration
--   in this case it should be in the type args of descurrdecl
-- a type name (that has to be in the environment) without type arugments
desugarTy (P.TyVar v) = do 
  ex <- tyExists v 
  if not ex then do  
    currDecl <- getCurrDecl (ErrVarUndefined v)
    case M.lookup v (M.fromList $ P.declArgs currDecl) of 
      Nothing -> throwError (ErrVarUndefined v)
      Just _ -> return $ D.TyVar v 
  else return $ D.TyDecl v [] 

-- this always has to be the current type or one that has been declared before
desugarTy (P.TyDecl tyn args) = do 
  args' <- forM args desugarTy 
  _ <- getTynPol tyn 
  return $ D.TyDecl tyn args' 
