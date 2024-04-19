module TypeCheck.Program (
  checkVarDecl
) where 

import TypeCheck.Definition (CheckM, addCheckerVar)
import TypeCheck.Errors (CheckerError(..))
import TypeCheck.Types (checkType)
import TypeCheck.Terms (checkTerm)
import Loc (Loc)
import Common (Variable)
import Syntax.Desugared.Program (VarDecl (..)) as D 
import Syntax.Desugared.Types (Ty) as D 
import Syntax.Typed.Program (VarDecl(..)) as T

import Prelude (bind,pure,($)) 
import Data.Maybe (Maybe(..))
import Data.Unit (unit)
import Control.Monad.Except (throwError)

getTypeAnnot :: Loc -> Variable -> Maybe D.Ty -> CheckM D.Ty
getTypeAnnot loc var Nothing = throwError (ErrNoAnnot loc var)
getTypeAnnot _ _ (Just ty) = pure ty

checkVarDecl :: D.VarDecl -> CheckM T.VarDecl
checkVarDecl (D.VarDecl var) =  do
  ty <- getTypeAnnot var.varPos var.varName var.varTy
  ty' <- checkType var.varPos ty 
  _ <- if var.varIsRec then addCheckerVar var.varName ty' else pure unit
  t' <- checkTerm var.varBody ty'
  pure $ T.VarDecl var {varBody=t',varTy=ty'}
