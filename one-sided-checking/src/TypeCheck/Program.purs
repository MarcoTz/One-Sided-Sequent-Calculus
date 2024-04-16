module TypeCheck.Program (
  checkVarDecl,
  checkRecDecl
) where 

import TypeCheck.Definition (CheckM, addCheckerVar)
import TypeCheck.Errors (CheckerError(..))
import TypeCheck.Types (checkType)
import TypeCheck.Terms (checkTerm)
import Loc (Loc)
import Common (Variable)
import Syntax.Desugared.Program (VarDecl (..),RecDecl(..)) as D 
import Syntax.Desugared.Types (Ty) as D 
import Syntax.Typed.Program (VarDecl(..), RecDecl(..)) as T

import Prelude (bind,pure,($)) 
import Data.Maybe (Maybe(..))
import Control.Monad.Except (throwError)

getTypeAnnot :: Loc -> Variable -> Maybe D.Ty -> CheckM D.Ty
getTypeAnnot loc var Nothing = throwError (ErrNoAnnot loc var)
getTypeAnnot _ _ (Just ty) = pure ty

checkVarDecl :: D.VarDecl -> CheckM T.VarDecl
checkVarDecl (D.VarDecl var) =  do
  ty <- getTypeAnnot var.varPos var.varName var.varTy
  ty' <- checkType var.varPos ty 
  t' <- checkTerm var.varBody ty'
  pure $ T.VarDecl {varPos:var.varPos, varName:var.varName,varTy:ty',varBody:t'}

checkRecDecl :: D.RecDecl -> CheckM T.RecDecl 
checkRecDecl (D.RecDecl rec) = do 
  ty <- getTypeAnnot rec.recPos rec.recName rec.recTy
  ty' <- checkType rec.recPos ty 
  _ <- addCheckerVar rec.recName ty'
  t' <- checkTerm rec.recBody ty'
  pure $ T.RecDecl {recPos:rec.recPos,recName:rec.recName,recTy:ty',recBody:t'}
