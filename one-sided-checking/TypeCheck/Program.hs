module TypeCheck.Program (
  checkVarDecl,
  checkRecDecl
) where 

import TypeCheck.Definition
import TypeCheck.Terms
import TypeCheck.Types
import Common
import Syntax.Desugared.Program qualified as D 
import Syntax.Typed.Program     qualified as T 
import Syntax.Typed.Terms       qualified as T
import Pretty.Common ()

import Control.Monad.Except
import Data.Map qualified as M

checkVarDecl :: D.VarDecl -> CheckM T.VarDecl
checkVarDecl (D.MkVar loc nm (Just ty) t) =  do
  ty1 <- tryCheckType loc ty (MkKind CBV)
  ty2 <- tryCheckType loc ty (MkKind CBN)
  t' <- case (ty1,ty2) of 
    (Right err1,Right err2) -> throwError (ErrList loc [err1,err2])
    (Left ty1',Left ty2') -> do 
      t1' <- tryCheckTerm t ty1'
      t2' <- tryCheckTerm t ty2'
      case (t1',t2') of 
        (Right err1, Right err2) -> throwError (ErrList loc [err1,err2])
        (Left t1'',_) -> return t1''
        (_,Left t2'') -> return t2''
    (Left ty1',_) -> checkTerm t ty1' 
    (_,Left ty2') -> checkTerm t ty2' 
  return $ T.MkVar loc nm (T.getType t') t'
checkVarDecl (D.MkVar loc nm Nothing _) = throwError (ErrNoAnnot loc nm)

checkRecDecl :: D.RecDecl -> CheckM T.RecDecl 
checkRecDecl (D.MkRec loc nm (Just ty) t) = do 
  ty1 <- tryCheckType loc ty (MkKind CBV)
  ty2 <- tryCheckType loc ty (MkKind CBN)
  t' <- case (ty1,ty2) of 
    (Right err1,Right err2) -> throwError (ErrList loc [err1,err2])
    (Left ty1',Left ty2') -> do 
      currVars <- getCheckerVars
      let newVars1 = M.insert nm ty1' currVars
      let newVars2 = M.insert nm ty2' currVars
      t1' <- withCheckerVars newVars1 (tryCheckTerm t ty1')
      t2' <- withCheckerVars newVars2 (tryCheckTerm t ty2')
      case (t1',t2') of 
        (Right err1, Right err2) -> throwError (ErrList loc [err1,err2])
        (Left t1'',_) -> return t1''
        (_,Left t2'') -> return t2''
    (Left ty1',_) -> addCheckerVar nm ty1' >> checkTerm t ty1'
    (_,Left ty2') -> addCheckerVar nm ty2' >> checkTerm t ty2'
  return $ T.MkRec loc nm (T.getType t') t'
checkRecDecl (D.MkRec loc nm Nothing _) = throwError (ErrNoAnnot loc nm)
