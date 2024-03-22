module Desugar.Program where 

import Errors
import Environment
import Desugar.Definition
import Desugar.Terms
import Desugar.Types
import Embed.Definition
import Embed.EmbedDesugared ()
import Syntax.Parsed.Program    qualified as P
import Syntax.Parsed.Terms      qualified as P
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
  forM_ (P.progRecs prog) desugarRec
  forM_ (P.progAnnots prog) desugarAnnot
  desugarMain (P.progMain prog)
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

desugarRec :: P.RecDecl -> DesugarM ()
desugarRec (P.MkRec v t) = do
  t' <- desugarTerm t 
  let newR = D.MkRec v Nothing t'
  addRec newR

desugarAnnot :: P.AnnotDecl -> DesugarM () 
desugarAnnot (P.MkAnnot v ty) = do 
  decl <- getDoneVar v
  ty' <- desugarPolTy ty
  case decl of 
    Left (D.MkVar _ Nothing t) -> addVar (D.MkVar v (Just ty') t)
    Left (D.MkVar _ (Just ty'') _) -> if ty' == ty'' then return () else throwError (ErrTypeNeq (embed ty'') (embed ty') "desugarAnnot")
    Right (D.MkRec _ Nothing t) -> addRec (D.MkRec v (Just ty') t)
    Right (D.MkRec _ (Just ty'') _) -> if ty' == ty'' then return () else throwError (ErrTypeNeq (embed ty'') (embed ty') "desugarAnnot")

desugarXtorSig :: P.XtorSig -> DesugarM D.XtorSig
desugarXtorSig (P.MkXtorSig xtn args) = do
  args' <- forM args desugarTy
  return (D.MkXtorSig xtn args')

desugarMain :: Maybe P.Command -> DesugarM  ()
desugarMain Nothing = return ()
desugarMain (Just c) = do 
  c' <- desugarCommand c 
  setMain c'
