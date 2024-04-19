module Dependencies.VariablesGraph (
  depOrderProgram
) where 

import Common (Variable(..), Xtorname(..))
import Environment (lookupMVar, lookupMXtor)
import Dependencies.Definition (DepM, removeSelfLoops, ensureAcyclic, addVertexM, getVertexError, addEdgeM)
import Dependencies.Errors (DepError(..))
import Dependencies.Graph (Graph(..), Vertex,getVertexLabel, getStartingVert, getEdgesEndingAt)
import Syntax.Parsed.Program (Program(..), DataDecl(..), VarDecl(..), XtorSig(..))
import Syntax.Parsed.Terms (Term(..),Pattern(..), Command(..))

import Prelude (bind, (<$>), (<>),pure,($), (||))
import Data.Unit (Unit,unit)
import Data.Foldable (foldM)
import Data.Traversable (for)
import Data.List (List(..),concat, concatMap,elem)
import Data.Tuple (Tuple(..),snd)
import Data.Map (toUnfoldable)
import Data.Maybe (isJust)
import Control.Monad.State (get)


type DepVar a = DepM Variable a

depOrderProgram :: Program -> DepVar (List Variable)
depOrderProgram (Program prog) = do 
  let vars :: List VarDecl 
      vars = snd <$> toUnfoldable prog.progVars
  let decls :: List DataDecl
      decls = snd <$> toUnfoldable prog.progDecls
  vertsTerms <- for vars addVariable
  let xtToVar (Xtorname xt) = Variable xt 
  let ignore = (\(XtorSig x) -> xtToVar (x.sigName)) <$> concatMap (\(DataDecl d) -> d.declXtors) decls  
  _ <- for vertsTerms (\(Tuple v t) -> addEdgesVariableT v ignore t)
  _ <- removeSelfLoops
  _ <- ensureAcyclic (ErrMutualRec prog.progName)
  order <- getVarOrder
  pure $ getVertexLabel <$> order

addVariable :: VarDecl -> DepVar (Tuple (Vertex Variable) Term)
addVariable (VarDecl var) = do
  vert <- addVertexM (var.varName)
  pure (Tuple vert var.varBody)

addEdgesVariableT :: Vertex Variable -> List Variable -> Term -> DepVar Unit
addEdgesVariableT vert ignore (Var loc var) = do 
  mdef <- lookupMVar var 
  mxtor <- lookupMXtor ((\(Variable v) -> Xtorname v) $ var)
  if isJust mdef || isJust mxtor || var `elem` ignore then pure unit
  else do
    vert' <- getVertexError var (ErrUndefinedVar loc var)
    addEdgeM (Tuple vert vert')
addEdgesVariableT vert ignore (Mu _ v c) = addEdgesVariableC vert (Cons v ignore) c 
addEdgesVariableT vert ignore (Xtor _ (Xtorname nm) args) = do
  _ <- for args (addEdgesVariableT vert (Cons (Variable nm) ignore))
  pure unit
addEdgesVariableT vert ignore (XCase _ pts) = do
  _ <- for pts (addEdgesVariablePt vert ignore)
  pure unit
addEdgesVariableT vert ignore (ShiftCBV _ t) = addEdgesVariableT vert ignore t
addEdgesVariableT vert ignore (ShiftCBN _ t) = addEdgesVariableT vert ignore t 

addEdgesVariablePt :: Vertex Variable -> List Variable -> Pattern -> DepVar Unit
addEdgesVariablePt vert ignore (Pattern pt) = do 
  let (Xtorname nm) = pt.ptxt
  addEdgesVariableC vert (pt.ptv <> (Cons (Variable nm) ignore)) pt.ptcmd

addEdgesVariableC :: Vertex Variable -> List Variable -> Command -> DepVar Unit
addEdgesVariableC vert ignore (Cut _ t _ u) = do
  _ <- addEdgesVariableT vert ignore t 
  _ <- addEdgesVariableT vert ignore u
  pure unit 
addEdgesVariableC vert ignore (CutAnnot _ t _ _ u) = do
  _ <- addEdgesVariableT vert ignore t 
  _ <- addEdgesVariableT vert ignore u 
  pure unit 
addEdgesVariableC _ _ (Done _) = pure unit
addEdgesVariableC _ _ (Err _ _) = pure unit
addEdgesVariableC vert ignore (Print _ t) = addEdgesVariableT vert ignore t  
addEdgesVariableC vert ignore (PrintAnnot _ t _) = addEdgesVariableT vert ignore t


getVarOrder :: DepVar (List (Vertex Variable))
getVarOrder = do 
  (MkGraph gr) <- get
  foldM (\doneOrder v -> if v `elem` doneOrder then pure doneOrder else (\x -> doneOrder <> x) <$> getVarOrderFrom v ) Nil (gr.grVerts) 

getVarOrderFrom :: Vertex Variable -> DepVar (List (Vertex Variable))
getVarOrderFrom vert = do 
  gr <- get 
  let preds = getStartingVert <$> getEdgesEndingAt vert gr
  case preds of 
    Nil -> pure (Cons vert Nil)
    _preds -> do
      predOrders <- for preds getVarOrderFrom 
      pure $ (Cons vert (concat predOrders))
