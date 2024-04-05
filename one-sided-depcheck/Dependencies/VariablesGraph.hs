module Dependencies.VariablesGraph (
  depOrderProgram
) where 

import Dependencies.Definition
import Dependencies.Graph
import Environment
import Common
import Syntax.Parsed.Program
import Syntax.Parsed.Terms

import Control.Monad
import Control.Monad.State
import Data.Map qualified as M
import Data.Maybe (isJust)

type DepVar a = DepM Variable a

depOrderProgram :: Program -> DepVar [Variable]
depOrderProgram (MkProgram mn decls vars recs _ _ _ _) = do 
  let vars' = snd <$> M.toList vars
  let recs' = snd <$> M.toList recs
  vertsTerms <- forM vars' addVariable
  recsTerms <- forM recs' addRec
  let ignore = (\(MkXtorName nm) -> MkVariable nm) . sigName <$> concatMap declXtors decls
  forM_ (vertsTerms++recsTerms) (\(v,t) -> addEdgesVariableT v ignore t)
  removeSelfLoops
  ensureAcyclic (ErrMutualRec mn)
  order <- getVarOrder
  return $ getVertexLabel <$> order

addVariable :: VarDecl -> DepVar (Vertex Variable,Term)
addVariable (MkVar _ v t) = do
  vert <- addVertexM v
  return (vert,t)

addRec :: RecDecl -> DepVar (Vertex Variable, Term)
addRec (MkRec _ v t) = do 
  vert <- addVertexM v 
  return (vert,t)

addEdgesVariableT :: Vertex Variable -> [Variable] -> Term -> DepVar ()
addEdgesVariableT vert ignore (Var loc v) = do 
  mdef <- lookupMVar v
  mxtor <- lookupMXtor ((\(MkVariable v') -> MkXtorName v') v)
  if isJust mdef || isJust mxtor || v `elem` ignore then return ()
  else do
    vert' <- getVertexError v (ErrUndefinedVar loc v)
    addEdgeM (vert,vert')
addEdgesVariableT vert ignore (Mu _ v c) = addEdgesVariableC vert (v:ignore) c 
addEdgesVariableT vert ignore (Xtor _ (MkXtorName nm) args) = forM_ args (addEdgesVariableT vert (MkVariable nm : ignore))
addEdgesVariableT vert ignore (XCase _ pts) = forM_ pts (addEdgesVariablePt vert ignore)
addEdgesVariableT vert ignore (ShiftPos _ t) = addEdgesVariableT vert ignore t
addEdgesVariableT vert ignore (ShiftNeg _ v c) = addEdgesVariableC vert (v:ignore) c 

addEdgesVariablePt :: Vertex Variable -> [Variable] -> Pattern -> DepVar ()
addEdgesVariablePt vert ignore (MkPattern (MkXtorName nm) vars c) = do 
  addEdgesVariableC vert (vars ++ (MkVariable nm : ignore)) c

addEdgesVariableC :: Vertex Variable -> [Variable] -> Command -> DepVar ()
addEdgesVariableC vert ignore (Cut _ t _ u) = do
  addEdgesVariableT vert ignore t 
  addEdgesVariableT vert ignore u
addEdgesVariableC vert ignore (CutAnnot _ t _ _ u) = do
  addEdgesVariableT vert ignore t 
  addEdgesVariableT vert ignore u 
addEdgesVariableC _ _ (Done _) = return ()
addEdgesVariableC _ _ (Err _ _) = return ()
addEdgesVariableC vert ignore (Print _ t) = addEdgesVariableT vert ignore t  
addEdgesVariableC vert ignore (PrintAnnot _ t _) = addEdgesVariableT vert ignore t


getVarOrder :: DepVar [Vertex Variable] 
getVarOrder = do 
  gr <- get
  foldM (\doneOrder v -> if v `elem` doneOrder then return doneOrder else (doneOrder ++ ) <$> getVarOrderFrom v ) [] (grVerts gr) 

getVarOrderFrom :: Vertex Variable -> DepVar [Vertex Variable]
getVarOrderFrom vert = do 
  gr <- get 
  let preds = getStartingVert <$> getEdgesEndingAt vert gr
  case preds of 
    [] -> return [vert]
    _preds -> do
      predOrders <- forM preds getVarOrderFrom 
      return $ vert:concat predOrders
