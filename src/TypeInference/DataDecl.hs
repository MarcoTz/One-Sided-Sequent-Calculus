module TypeInference.DataDecl where 

import Untyped.Program qualified as S 
import Typed.Program qualified as T 
import Typed.Types qualified as T
import Common

import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import Data.Map qualified as M

data DeclState = MkDeclState { declVars :: !(M.Map Variable Pol), declPol :: !Pol }

initialDeclState :: DeclState
initialDeclState = MkDeclState M.empty Pos

newtype DeclM a = DeclM { getDeclM :: StateT DeclState (Except String) a }
  deriving newtype (Functor, Applicative, Monad, MonadState DeclState, MonadError String)


runDeclM :: DeclM a -> Either String a 
runDeclM m = case runExcept (runStateT (getDeclM m) initialDeclState) of
  Left err -> Left err 
  Right (a,_) ->  Right a 

setSt :: [(Variable,Pol)] -> Pol -> DeclM () 
setSt vars pol = do 
  let newVs = foldr (\(v,p) m -> M.insert v p m) M.empty vars
  modify (\_ -> MkDeclState newVs pol)

checkDecls :: [S.DataDecl] -> DeclM [T.DataDecl]
checkDecls decls = do 
  let declNms = S.declNm <$> decls 
  let declXtors = S.sigName <$> concatMap S.declSig decls
  case (checkDups declNms, checkDups declXtors) of 
    (Just tn,_) -> throwError ("Type " <> tn <> " declared multiple times")
    (_,Just xtn) -> throwError ("Xtor " <> xtn <> " declared multiple times")
    (Nothing,Nothing) -> forM decls inferDecl
  where 
    checkDups :: Eq a => [a] -> Maybe a 
    checkDups [] = Nothing
    checkDups (tn:tns) = if tn `elem` tns then Just tn else checkDups tns

inferDecl :: S.DataDecl -> DeclM T.DataDecl
inferDecl (S.MkDataDecl nm tyargs pol xts) = do 
  setSt tyargs pol
  newXts <- forM xts inferSig 
  return $ T.MkDataDecl nm tyargs pol newXts

inferSig :: S.XtorSig -> DeclM T.XtorSig 
inferSig (S.MkXtorSig nm args) = do
  newTys <- forM args inferTy
  return $  T.MkXtorSig nm newTys 

inferTy :: S.Ty -> DeclM T.Ty
inferTy (S.TyVar v) = do 
  vars <- gets declVars 
  case M.lookup v vars of 
    Nothing -> throwError ("Variable " <> v <> " used but not defined")
    Just pol -> return $ T.TyVar v (T.MkKind pol)
inferTy (S.TyDecl nm args) = do 
  newArgs <- forM args inferTy 
  pol <- gets declPol
  return $ T.TyDecl nm newArgs (T.MkKind pol)
