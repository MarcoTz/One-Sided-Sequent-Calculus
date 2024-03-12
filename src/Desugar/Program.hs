module Desugar.Program where 

import Errors
import Environment
import Desugar.Definition
import Desugar.Terms
import Desugar.Types
import Embed.Definition
import Embed.EmbedDesugared ()
import Syntax.Parsed.Program    qualified as P
import Syntax.Desugared.Program qualified as D

import Control.Monad.Except
import Control.Monad.State
import Control.Monad
import Data.Map qualified as M


checkNames :: Eq a => [a] -> (a -> Error) -> DesugarM () 
checkNames [] _ = return ()
checkNames (nm1:nms) err = if nm1 `elem` nms then throwError (err nm1) else checkNames nms err


desugarProgram :: P.Program -> DesugarM D.Program
desugarProgram prog = do 
  let decls = P.progDecls prog
  envTyNames <- getTypeNames
  let declNames = (fst <$> M.toList decls) ++ envTyNames 
  checkNames declNames (`ErrDuplDecl` "desugarProgram")
  envXtns <- getXtorNames
  let xtns = (P.sigName <$> concatMap P.declXtors decls) ++ envXtns
  checkNames xtns (`ErrDuplXtor` "desugarProgram")
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
  ty' <- desugarPolTy ty
  case mty of 
    Nothing -> addVar (D.MkVar v (Just ty') t)
    Just ty'' -> if ty' == ty'' then return () else throwError (ErrTypeNeq (embed . fst $ ty'') (embed . fst $ ty') "desugarAnnot")

desugarXtorSig :: P.XtorSig -> DesugarM D.XtorSig
desugarXtorSig (P.MkXtorSig xtn args) = do
  args' <- forM args desugarTy
  return (D.MkXtorSig xtn args')
