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

checkVarDecl :: D.VarDecl -> CheckM T.VarDecl
checkVarDecl (D.MkVar loc nm (Just ty) t) =  do
  ty1 <- tryCheckType loc ty (MkKind CBV)
  ty2 <- tryCheckType loc ty (MkKind CBN)
  ty' <- case (ty1,ty2) of 
    (Right err1,Right err2) -> throwError (ErrList loc [err1,err2])
    (Left ty1',_) -> return ty1'
    (_,Left ty2') -> return ty2'
  t' <- checkTerm t ty'
  return $ T.MkVar loc nm (T.getType t') t'
checkVarDecl (D.MkVar loc nm Nothing _) = throwError (ErrNoAnnot loc nm)

checkRecDecl :: D.RecDecl -> CheckM T.RecDecl 
checkRecDecl (D.MkRec loc nm (Just ty) t) = do 
  ty1 <- tryCheckType loc ty (MkKind CBV)
  ty2 <- tryCheckType loc ty (MkKind CBN)
  ty' <- case (ty1,ty2) of 
    (Right err1,Right err2) -> throwError (ErrList loc [err1,err2])
    (Left ty1',_) -> return ty1'
    (_,Left ty2') -> return ty2'
  addCheckerVar nm ty'
  t' <- checkTerm t ty' 
  return $ T.MkRec loc nm (T.getType t') t'
checkRecDecl (D.MkRec loc nm Nothing _) = throwError (ErrNoAnnot loc nm)
