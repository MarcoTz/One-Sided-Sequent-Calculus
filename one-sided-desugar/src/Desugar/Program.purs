module Desugar.Program (  
  desugarProgram
) where 

import Common (Typename, Xtorname)
import Environment (getTypeNames,getXtorNames)
import Desugar.Definition (DesugarM,setDesCurrDecl, getDesDoneProg, addDesDecl, addDesVar,addDesRec, getDesDoneVar, setDesMain)
import Desugar.Terms (desugarTerm,desugarCommand)
import Desugar.Types (desugarTy)
import Desugar.Errors (DesugarError(..))
import Syntax.Parsed.Program (Program(..),DataDecl(..),VarDecl(..),RecDecl(..),XtorSig(..), AnnotDecl(..)) as P
import Syntax.Parsed.Terms (Command) as P
import Syntax.Desugared.Program (Program,DataDecl(..),VarDecl(..),RecDecl(..),XtorSig(..)) as D

import Prelude (bind,pure, (<$>), (||),(==))
import Data.List (List(..),elem, concatMap)
import Data.Unit (Unit,unit)
import Data.Tuple (snd)
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
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
  _ <- for prog.progRecs desugarRec
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
  let newV = D.VarDecl {varPos:var.varPos, varName:var.varName, varTy:Nothing, varBody:t'}
  addDesVar newV

desugarRec :: P.RecDecl -> DesugarM Unit
desugarRec (P.RecDecl rec) = do
  t' <- desugarTerm rec.recBody
  let newR = D.RecDecl {recPos:rec.recPos,recName:rec.recName,recTy:Nothing,recBody:t'}
  addDesRec newR

desugarAnnot :: P.AnnotDecl -> DesugarM Unit
desugarAnnot (P.AnnotDecl annot) = do 
  decl <- getDesDoneVar annot.annotPos annot.annotName
  ty' <- desugarTy annot.annotType
  case decl of 
    Left (D.VarDecl {varPos:loc',varName:v, varTy:Nothing,varBody:t}) -> 
      addDesVar (D.VarDecl {varPos:loc',varName:v, varTy:Just ty', varBody:t})
    Left (D.VarDecl {varPos:_, varName:v, varTy:Just ty'', varBody:_}) -> 
      if ty' == ty'' then pure unit else throwError (ErrMultipleAnnot annot.annotPos v ty' ty'')
    Right (D.RecDecl {recPos:loc', recName:v, recTy:Nothing, recBody:t}) -> 
      addDesRec (D.RecDecl {recPos:loc',recName:v,recTy:(Just ty'),recBody:t})
    Right (D.RecDecl {recPos:_, recName:_, recTy:Just ty'', recBody:_}) -> 
      if ty' == ty'' then pure unit else throwError (ErrMultipleAnnot annot.annotPos annot.annotName ty' ty'')

desugarXtorSig :: P.XtorSig -> DesugarM D.XtorSig
desugarXtorSig (P.XtorSig sig) = do
  args' <- for sig.sigArgs desugarTy
  pure (D.XtorSig {sigPos:sig.sigPos,sigName:sig.sigName, sigArgs:args'})

desugarMain :: Maybe P.Command -> DesugarM  Unit
desugarMain Nothing = pure unit
desugarMain (Just c) = do 
  c' <- desugarCommand c 
  setDesMain c'
