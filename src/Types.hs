module Types where 

import Syntax
import Data.Map qualified as M

type TypeVar = String
type TypeName = String 
data Ty = 
  TyVar !TypeVar 
  | TyDecl !TypeName ![Ty] 
  | TyShift !Ty
  | TyCo !Ty
  deriving (Eq)

data Decl = 
  MkDataDecl{declNm :: !TypeName, declArgs :: ![(Variable,Pol)], declPol :: !Pol, declSig :: ![XtorSig]} 
  | MkValDecl{valVar :: !Variable, valTy :: !Ty, valBd :: !Term}
  | MkRecDecl{recVar :: !Variable, recTy :: !Ty, recBd :: !Term}
  | MkEps
  | MkCoDecl !Decl

data XtorSig = MkXtorSig{sigName :: !XtorName, sigArgs :: ![Ty]} 

data Env = MkEnv { envVars :: !(M.Map Variable (Pol, Ty)), envTyVars :: !(M.Map TypeVar Pol), envDecls :: ![Decl] }

findDataDecl :: Env -> XtorName -> Maybe Decl 
findDataDecl env nm = findDecl nm (envDecls env)
  where 
    findDecl :: XtorName -> [Decl] -> Maybe Decl
    findDecl _ [] = Nothing
    findDecl nm' (d@MkDataDecl{declNm=_,declArgs=_,declPol=_,declSig=xtors}:decls) = 
      if not (any (\xt -> sigName xt==nm') xtors) then findDecl nm' decls else Just d
    findDecl nm' (_:decls) = findDecl nm' decls

freshTyVar :: Int -> Env -> TypeVar
freshTyVar n env@MkEnv{envVars = _, envTyVars = tyVars, envDecls=_} = 
  let newV = "x" <> show n in
  case M.lookup newV tyVars of 
    Just _ -> freshTyVar (n+1) env
    Nothing -> newV 

typeCommand :: Env -> Command -> Bool
typeCommand env (Cut t pol u) = 
  let ty1 = typeTerm env t 
      ty2 = typeTerm env u in
  case (ty1,ty2) of 
    (Nothing,_) -> False 
    (_,Nothing) -> False
    (Just (pol',ty), Just (pol'',ty')) -> case kindType env ty of 
      Nothing -> False 
      Just pol''' -> ty == ty' && pol' == flipPol pol'' && multPol pol''' pol' == pol 



typeTerm :: Env -> Term -> Maybe (Pol,Ty)
typeTerm env (Var v) = M.lookup v (envVars env)
-- This only works with Pos ri$ght now, adjust so both works (but first refactor)
typeTerm env (Mu v st) = 
  let frT = freshTyVar 0 env
      newVEnv = M.insert v (Pos,TyVar frT) (envVars env)
      newEnv = MkEnv{envVars = newVEnv, envTyVars=envTyVars env, envDecls = envDecls env} in
  if typeCommand newEnv st then Just (Neg,TyVar frT) else Nothing
-- rules CoI and CoE are not included
typeTerm env (Shift t) = 
  case typeTerm env t of 
    Nothing -> Nothing 
    Just (Pos, ty) -> Just (Pos, TyShift ty)
    Just (Neg,_) -> Nothing
typeTerm env (Lam v c) = 
  let frT = freshTyVar 0 env
      newVEnv = M.insert v (Pos,TyVar frT) (envVars env)
      newEnv = MkEnv{envVars=newVEnv, envTyVars=envTyVars env, envDecls=envDecls env} in
  if typeCommand newEnv c then Just (Neg,TyShift (TyVar frT)) else Nothing
-- Rule F1 also missing (requires checking signatures)
typeTerm _ _ = Nothing

kindType :: Env -> Ty -> Maybe Pol 
kindType _ _ = Just Pos
