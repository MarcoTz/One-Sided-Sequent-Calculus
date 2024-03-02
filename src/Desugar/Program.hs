module Desugar.Program where 

import Common
import Errors
import Environment
import Desugar.Definition
import Desugar.Terms
import Syntax.Parsed.Program    qualified as P
import Syntax.Parsed.Types      qualified as P
import Syntax.Desugared.Program qualified as D
import Syntax.Desugared.Types   qualified as D

import Control.Monad.Except
import Control.Monad.State
import Control.Monad
import Data.Map qualified as M
import Data.List (find)


checkNames :: Eq a => [a] -> (a -> Error) -> DesugarM () 
checkNames [] _ = return ()
checkNames (nm1:nms) err = if nm1 `elem` nms then throwError (err nm1) else checkNames nms err


desugarProgram :: P.Program -> DesugarM D.Program
desugarProgram prog = do 
  let decls = P.progDecls prog
  envTyNames <- getTypeNames
  let declNames = (fst <$> M.toList decls) ++ envTyNames 
  checkNames declNames (`ErrDuplDecl` WhereDesugar)
  envXtns <- getXtorNames
  let xtns = (P.sigName <$> concatMap P.declXtors decls) ++ envXtns
  checkNames xtns (`ErrDuplXtor` WhereDesugar)
  forM_ decls desugarDecl 
  forM_ (P.progVars prog) desugarVar
  forM_ (P.progAnnots prog) desugarAnnot
  gets desDone
    

desugarDecl :: P.DataDecl -> DesugarM () 
desugarDecl d@(P.MkData tyn tyargs  pol sigs)= do 
  setCurrDecl d
  sigs' <- forM sigs desugarXtorSig
  let newD = D.MkData tyn tyargs pol sigs'
  addDecl newD

desugarVar :: P.VarDecl -> DesugarM () 
desugarVar (P.MkVar v t) = do 
  t' <- desugarTerm t
  let newV = D.MkVar v Nothing t'
  addVar newV

desugarAnnot :: P.AnnotDecl -> DesugarM () 
desugarAnnot (P.MkAnnot v ty) = do 
  D.MkVar _ mty t <- getDoneVar v
  ty' <- desugarTypeScheme ty
  case mty of 
    Nothing -> addVar (D.MkVar v (Just ty') t)
    Just ty'' -> if ty' == ty'' then return () else throwError (ErrTypeSchemeNeq ty'' ty' WhereDesugar)

desugarXtorSig :: P.XtorSig -> DesugarM D.XtorSig
desugarXtorSig (P.MkXtorSig xtn args) = do
  args' <- forM args desugarTy
  return (D.MkXtorSig xtn args')

desugarTypeScheme :: P.TypeScheme -> DesugarM D.TypeScheme
desugarTypeScheme (P.MkTypeScheme vars ty) = do 
  ty' <- desugarTypeSchemeTy ty vars
  return $ D.MkTypeScheme vars ty' 
  where 
    desugarTypeSchemeTy :: P.Ty -> [PolVar] -> DesugarM D.Ty
    desugarTypeSchemeTy (P.TyVar v) tyVars = do
      let vty = tyvarToTyName v
      let vars' = (\(MkPolVar tyv _) -> tyv) <$> tyVars
      mdecl <- getMDecl vty
      case mdecl of 
        Just _ -> return $ D.TyDecl vty [] 
        Nothing -> if v `elem` vars' then return $ D.TyVar v else throwError (ErrMissingTyVar v WhereDesugar)
    desugarTypeSchemeTy (P.TyDecl tyn args) tyVars = do 
      args' <- forM args (`desugarTypeSchemeTy` tyVars) 
      return $ D.TyDecl tyn args' 

desugarTy :: P.Ty -> DesugarM D.Ty
-- a type variable appearing in  a declaration is either 
-- an actual variable that is the argument of the current declaration
--   in this case it should be in the type args of descurrdecl
-- a type name (that has to be in the environment) without type arugments
desugarTy (P.TyVar v) = do 
  let vty = tyvarToTyName v
  mdecl <- getMDecl vty
  case mdecl of 
    Nothing -> do 
      currDecl <- getCurrDecl (ErrMissingDecl vty WhereDesugar)
      case find (\(MkPolVar v' _) -> v'==v) (P.declArgs currDecl) of 
        Nothing -> throwError (ErrMissingDecl vty WhereDesugar)
        Just _ -> return $ D.TyVar v 
    Just _ -> return $ D.TyDecl vty [] 

-- this always has to be the current type or one that has been declared before
desugarTy (P.TyDecl tyn args) = do 
  args' <- forM args desugarTy 
  return $ D.TyDecl tyn args' 
