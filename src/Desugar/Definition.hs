module Desugar.Definition where 

import Errors 
import Common 
import Syntax.Desugared.Program
import Syntax.Parsed.Program qualified as P

import Control.Monad.State
import Control.Monad.Except
import Data.Map qualified as M


data DesugarState = MkDesugarState { desDecls :: !(M.Map TypeName DataDecl), desCurrDecl :: !(Maybe P.DataDecl)} 

initialDesugarState :: DesugarState 
initialDesugarState = MkDesugarState M.empty Nothing

newtype DesugarM a = DesugarM { getDesugarM :: StateT DesugarState (Except Error) a }
  deriving newtype (Functor, Applicative, Monad, MonadState DesugarState, MonadError Error)

runDesugarM :: DesugarM a -> Either Error a
runDesugarM m = case runExcept (runStateT (getDesugarM m) initialDesugarState) of
  Left err -> Left err 
  Right (x,_) ->  Right x 

addDataDecl :: DataDecl -> DesugarM () 
addDataDecl decl = modify (\s -> MkDesugarState (M.insert (declNm decl) decl (desDecls s)) (desCurrDecl s))

setCurrDecl :: P.DataDecl -> DesugarM () 
setCurrDecl decl = modify (\s -> MkDesugarState (desDecls s) (Just decl))

getCurrDecl :: Error -> DesugarM P.DataDecl
getCurrDecl err = do 
  curr <- gets desCurrDecl 
  case curr of 
    Nothing -> throwError err 
    Just decl -> return decl

getTynPol :: TypeName -> DesugarM Pol 
getTynPol tyn = do 
  decls <- gets desDecls 
  case M.lookup tyn decls of 
    Just decl ->  return $ declPol decl
    Nothing -> do 
      curr <- getCurrDecl (ErrDeclUndefined tyn)
      return $ P.declPol curr

getDeclNames :: DesugarM [TypeName]
getDeclNames = do 
  decls <- gets desDecls
  return (fst <$> M.toList decls)
