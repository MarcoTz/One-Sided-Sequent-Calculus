module Desugar.Program (  
  desugarProgram
) where 

import Environment
import Common
import Loc 
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


checkTypeNames :: [Typename] -> [P.DataDecl] -> DesugarM ()
checkTypeNames _ [] = return ()
checkTypeNames tyns (decl1:decls) = let tyn = P.declName decl1 in 
  if tyn `elem` (P.declName <$> decls) || tyn `elem` tyns then 
    throwError (ErrMultipleNames (getLoc decl1) (P.declName decl1)) else  checkTypeNames tyns decls

checkXtorNames :: [Xtorname] -> [P.XtorSig] -> DesugarM () 
checkXtorNames _ [] = return () 
checkXtorNames xtns (xt1:xts) = let xtn = P.sigName xt1 in 
  if xtn `elem` (P.sigName <$> xts) || xtn `elem` xtns then throwError (ErrMultipleXtor (getLoc xt1) xtn) else checkXtorNames xtns xts

desugarProgram :: P.Program -> DesugarM D.Program
desugarProgram prog = do 
  let decls = P.progDecls prog
  envTyNames <- getTypeNames
  checkTypeNames envTyNames (snd <$> M.toList decls)
  envXtns <- getXtorNames
  checkXtorNames envXtns (concatMap P.declXtors decls)
  forM_ decls desugarDecl 
  forM_ (P.progVars prog) desugarVar
  forM_ (P.progRecs prog) desugarRec
  forM_ (P.progAnnots prog) desugarAnnot
  desugarMain (P.progMain prog)
  getDesDoneProg
    

desugarDecl :: P.DataDecl -> DesugarM () 
desugarDecl d@(P.MkData loc tyn tyargs  pol sigs)= do 
  setDesCurrDecl d
  sigs' <- forM sigs desugarXtorSig
  let newD = D.MkData loc tyn tyargs pol sigs'
  addDesDecl newD

desugarVar :: P.VarDecl -> DesugarM () 
desugarVar (P.MkVar loc v t) = do 
  t' <- desugarTerm t
  let newV = D.MkVar loc v Nothing t'
  addDesVar newV

desugarRec :: P.RecDecl -> DesugarM ()
desugarRec (P.MkRec loc v t) = do
  t' <- desugarTerm t 
  let newR = D.MkRec loc v Nothing t'
  addDesRec newR

desugarAnnot :: P.AnnotDecl -> DesugarM () 
desugarAnnot (P.MkAnnot loc v ty) = do 
  decl <- getDesDoneVar loc v
  ty' <- desugarTy ty
  case decl of 
    Left (D.MkVar loc' _ Nothing t) -> addDesVar (D.MkVar loc' v (Just ty') t)
    Left (D.MkVar _ _ (Just ty'') _) -> if ty' == ty'' then return () else throwError (ErrMultipleAnnot loc v ty' ty'')
    Right (D.MkRec loc' _ Nothing t) -> addDesRec (D.MkRec loc' v (Just ty') t)
    Right (D.MkRec _ _ (Just ty'') _) -> if ty' == ty'' then return () else throwError (ErrMultipleAnnot loc v ty' ty'')

desugarXtorSig :: P.XtorSig -> DesugarM D.XtorSig
desugarXtorSig (P.MkXtorSig loc xtn args) = do
  args' <- forM args desugarTy
  return (D.MkXtorSig loc xtn args')

desugarMain :: Maybe P.Command -> DesugarM  ()
desugarMain Nothing = return ()
desugarMain (Just c) = do 
  c' <- desugarCommand c 
  setDesMain c'
