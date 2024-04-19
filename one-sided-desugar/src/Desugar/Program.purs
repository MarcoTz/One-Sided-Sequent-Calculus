module Desugar.Program (  
  desugarProgram
) where 

import Common (Typename, Xtorname)
import Environment (getTypeNames,getXtorNames)
import Desugar.Definition (DesugarM,setDesCurrDecl, getDesDoneProg, addDesDecl, addDesVar, getDesDoneVar, setDesMain)
import Desugar.Terms (desugarTerm,desugarCommand)
import Desugar.Types (desugarTy)
import Desugar.Errors (DesugarError(..))
import Syntax.Parsed.Program (Program(..),DataDecl(..),VarDecl(..),XtorSig(..), AnnotDecl(..)) as P
import Syntax.Parsed.Terms (Command) as P
import Syntax.Desugared.Program (Program,DataDecl(..),VarDecl(..),XtorSig(..)) as D

import Prelude (bind,pure, (<$>), (||),(==),($))
import Data.List (List(..),elem, concatMap)
import Data.Unit (Unit,unit)
import Data.Tuple (snd)
import Data.Maybe (Maybe(..))
import Data.Map (toUnfoldable)
import Data.Traversable (for)
import Control.Monad.Except (throwError)

checkTypeNames :: List Typename -> List P.DataDecl -> DesugarM Unit
checkTypeNames _ Nil = pure unit
checkTypeNames tyns (Cons (P.DataDecl decl1) decls) = let tyn = decl1.declName in 
  if tyn `elem` ((\(P.DataDecl x) -> x.declName) <$> decls) || tyn `elem` tyns then 
    throwError (ErrMultipleNames (decl1.declPos) decl1.declName) else  checkTypeNames tyns decls

checkXtorNames :: List Xtorname -> List P.XtorSig -> DesugarM Unit
checkXtorNames _ Nil = pure unit 
checkXtorNames xtns (Cons (P.XtorSig xt1) xts) = let xtn = xt1.sigName in 
  if xtn `elem` ((\(P.XtorSig x) -> x.sigName) <$> xts) || xtn `elem` xtns then throwError (ErrMultipleXtor xt1.sigPos xtn) else checkXtorNames xtns xts

desugarProgram :: P.Program -> DesugarM D.Program
desugarProgram (P.Program prog) = do 
  let decls = prog.progDecls 
  envTyNames <- getTypeNames
  _ <- checkTypeNames envTyNames (snd <$> toUnfoldable decls)
  envXtns <- getXtorNames
  _ <- checkXtorNames envXtns (concatMap (\(P.DataDecl x) -> x.declXtors) (snd <$> toUnfoldable prog.progDecls))
  _ <- for decls desugarDecl 
  _ <- for prog.progVars desugarVar
  _ <- for prog.progAnnots desugarAnnot
  _ <- desugarMain prog.progMain
  getDesDoneProg
    

desugarDecl :: P.DataDecl -> DesugarM Unit 
desugarDecl d@(P.DataDecl decl)= do 
  _ <- setDesCurrDecl d
  sigs' <- for decl.declXtors desugarXtorSig
  let newD = D.DataDecl {declPos:decl.declPos,declName:decl.declName,declArgs:decl.declArgs,declType:decl.declType,declXtors:sigs'}
  addDesDecl newD

desugarVar :: P.VarDecl -> DesugarM Unit 
desugarVar (P.VarDecl var) = do 
  t' <- desugarTerm var.varBody
  let newVar = {varPos:var.varPos,varName:var.varName,varIsRec:var.varIsRec,varTy:Nothing,varBody:t'}
  addDesVar $ D.VarDecl newVar 

desugarAnnot :: P.AnnotDecl -> DesugarM Unit
desugarAnnot (P.AnnotDecl annot) = do 
  (D.VarDecl decl) <- getDesDoneVar annot.annotPos annot.annotName
  ty' <- desugarTy annot.annotType
  case decl.varTy of 
      Nothing -> addDesVar (D.VarDecl decl{varTy=Just ty'})
      Just ty'' -> if ty'==ty'' then pure unit else throwError (ErrMultipleAnnot annot.annotPos decl.varName ty' ty'')

desugarXtorSig :: P.XtorSig -> DesugarM D.XtorSig
desugarXtorSig (P.XtorSig sig) = do
  args' <- for sig.sigArgs desugarTy
  pure (D.XtorSig {sigPos:sig.sigPos,sigName:sig.sigName, sigArgs:args'})

desugarMain :: Maybe P.Command -> DesugarM  Unit
desugarMain Nothing = pure unit
desugarMain (Just c) = do 
  c' <- desugarCommand c 
  setDesMain c'
