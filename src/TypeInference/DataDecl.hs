module TypeInference.DataDecl where 

import Untyped.Program qualified as S 
import Typed.Program qualified as T 
import Typed.Types qualified as T
import Common



checkDecls :: [S.DataDecl] -> [T.DataDecl]
checkDecls decls = do 
  let declNms = S.declNm <$> decls 
  let declXtors = S.sigName <$> concatMap S.declSig decls
  case (checkDups declNms, checkDups declXtors) of 
    (Just tn,_) -> error ("Type " <> tn <> " declared multiple times")
    (_,Just xtn) -> error ("Xtor " <> xtn <> " declared multiple times")
    (Nothing,Nothing) -> inferDecl <$> decls
  where 
    checkDups :: Eq a => [a] -> Maybe a 
    checkDups [] = Nothing
    checkDups (tn:tns) = if tn `elem` tns then Just tn else checkDups tns

inferDecl :: S.DataDecl -> T.DataDecl
inferDecl (S.MkDataDecl nm tyargs pol xts) = T.MkDataDecl nm tyargs pol (inferSig tyargs pol <$> xts)

inferSig :: [(Variable,Pol)] -> Pol -> S.XtorSig -> T.XtorSig 
inferSig vars pol (S.MkXtorSig nm args) = T.MkXtorSig nm (inferTy vars pol <$> args)

inferTy :: [(Variable,Pol)] -> Pol -> S.Ty -> T.Ty
inferTy vars _ (S.TyVar v) = 
  case findVar vars v of 
    Nothing -> error "Variable not defined" 
    Just pol -> T.TyVar v (T.MkKind pol)
inferTy vars pol (S.TyDecl n args) = T.TyDecl n (inferTy vars pol <$> args) (T.MkKind pol)

findVar :: [(Variable, Pol)] -> Variable -> Maybe Pol 
findVar [] _ = Nothing 
findVar ((v1,pl1):vars) var = if v1 == var then Just pl1 else findVar vars var
