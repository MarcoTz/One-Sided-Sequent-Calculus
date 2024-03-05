module TypeCheck.Types where 

import TypeCheck.Definition
import Syntax.Desugared.Types qualified as D
import Syntax.Typed.Types     qualified as T
import Syntax.Typed.Program   qualified as T
import Errors
import Common
import Environment

import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import Data.Maybe (fromMaybe, isNothing)
import Data.Map qualified as M


checkType :: D.Ty -> Maybe Pol -> CheckM T.Ty
checkType (D.TyVar v) mpol = do
  tyVars <- gets checkTyVars
  case M.lookup v tyVars of 
    Nothing -> throwError (ErrMissingTyVar v "checkType TyVar")
    Just pol -> if isNothing mpol || Just pol == mpol then return (T.TyVar v pol) else  throwError (ErrKind ShouldEq "checkType TyVar")

checkType (D.TyDecl tyn args) mpol = do 
   T.MkDataDecl _ argVars pol' _ <- lookupDecl tyn
   polPairs <- zipWithError args (Just . getKind <$> argVars) (ErrTyArity tyn " checkType TyDecl")
   args' <- forM polPairs (uncurry checkType)
   let newPol = fromMaybe pol' mpol
   return $ T.TyDecl tyn args' newPol

checkType (D.TyCo ty) mpol = T.TyCo <$> checkType ty (flipPol <$> mpol)
