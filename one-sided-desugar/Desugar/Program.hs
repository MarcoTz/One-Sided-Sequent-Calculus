module Desugar.Program (  
  desugarProgram
) where 

import Environment
import Desugar.Definition
import Desugar.Terms
import Desugar.Types
import Embed.EmbedDesugared ()
import Syntax.Parsed.Program    qualified as P
import Syntax.Parsed.Terms      qualified as P
import Syntax.Desugared.Program qualified as D

import Control.Monad.Except
import Control.Monad
import Data.Map qualified as M

checkNames :: Eq a => Show a => [a] -> DesugarM () 
checkNames [] = return ()
checkNames (nm1:nms) = if nm1 `elem` nms then throwError (ErrMultiple (show nm1)) else checkNames nms 


desugarProgram :: P.Program -> DesugarM D.Program
desugarProgram prog = do 
  let decls = P.progDecls prog
  envTyNames <- getTypeNames
  let declNames = (fst <$> M.toList decls) ++ envTyNames 
  checkNames declNames 
  envXtns <- getXtorNames
  let xtns = (P.sigName <$> concatMap P.declXtors decls) ++ envXtns
  checkNames xtns 
  forM_ decls desugarDecl 
  forM_ (P.progVars prog) desugarVar
  forM_ (P.progRecs prog) desugarRec
  forM_ (P.progAnnots prog) desugarAnnot
  desugarMain (P.progMain prog)
  getDesDoneProg
    

desugarDecl :: P.DataDecl -> DesugarM () 
desugarDecl d@(P.MkData tyn tyargs  pol sigs)= do 
  setDesCurrDecl d
  sigs' <- forM sigs desugarXtorSig
  let newD = D.MkData tyn tyargs pol sigs'
  addDesDecl newD

desugarVar :: P.VarDecl -> DesugarM () 
desugarVar (P.MkVar v t) = do 
  t' <- desugarTerm t
  let newV = D.MkVar v Nothing t'
  addDesVar newV

desugarRec :: P.RecDecl -> DesugarM ()
desugarRec (P.MkRec v t) = do
  t' <- desugarTerm t 
  let newR = D.MkRec v Nothing t'
  addDesRec newR

desugarAnnot :: P.AnnotDecl -> DesugarM () 
desugarAnnot (P.MkAnnot v ty) = do 
  decl <- getDesDoneVar v
  ty' <- desugarPolTy ty
  case decl of 
    Left (D.MkVar _ Nothing t) -> addDesVar (D.MkVar v (Just ty') t)
    Left (D.MkVar _ (Just ty'') _) -> if ty' == ty'' then return () else throwError (ErrMultipleAnnot v ty' ty'')
    Right (D.MkRec _ Nothing t) -> addDesRec (D.MkRec v (Just ty') t)
    Right (D.MkRec _ (Just ty'') _) -> if ty' == ty'' then return () else throwError (ErrMultipleAnnot v ty' ty'')

desugarXtorSig :: P.XtorSig -> DesugarM D.XtorSig
desugarXtorSig (P.MkXtorSig xtn args) = do
  args' <- forM args desugarTy
  return (D.MkXtorSig xtn args')

desugarMain :: Maybe P.Command -> DesugarM  ()
desugarMain Nothing = return ()
desugarMain (Just c) = do 
  c' <- desugarCommand c 
  setDesMain c'
