module Dependencies.VariablesGraph where 

import Dependencies.Definition
import Common
import Errors
import Syntax.Parsed.Program
import Syntax.Parsed.Terms

import Control.Monad
import Control.Monad.State
import Data.Map qualified as M

import Debug.Trace 
import Pretty.Common ()
type DepVar a = DepM Variable a

depOrderProgram :: Program -> DepVar [Variable]
depOrderProgram (MkProgram mn decls vars _ _) = do 
  let vars' = snd <$> M.toList vars
  vertsTerms <- forM vars' addVariable
  let ignore = (\(MkXtorName nm) -> MkVariable nm) . sigName <$> concatMap declXtors decls
  forM_ vertsTerms (\(v,t) -> addEdgesVariableT v ignore t)
  gr <- get 
  trace (show gr) $ return ()
  removeSelfLoops
  ensureAcyclic (ErrMutualRec mn "depOrderProgram")
  order <- getVarOrder
  return $ getVertexLabel <$> order

addVariable :: VarDecl -> DepVar (Vertex Variable,Term)
addVariable (MkVar v t) = do
  vert <- addVertexM v
  return (vert,t)

addEdgesVariableT :: Vertex Variable -> [Variable] -> Term -> DepVar ()
addEdgesVariableT vert ignore (Var v) = 
  if v `elem` ignore then return () else do
    vert' <- getVertexError v (ErrMissingVar v "addEdgesVariable")
    addEdgeM (vert,vert')
addEdgesVariableT vert ignore (Mu v c) = addEdgesVariableC vert (v:ignore) c 
addEdgesVariableT vert ignore (Xtor (MkXtorName nm) args) = forM_ args (addEdgesVariableT vert (MkVariable nm : ignore))
addEdgesVariableT vert ignore (XCase pts) = forM_ pts (addEdgesVariablePt vert ignore)
addEdgesVariableT vert ignore (ShiftPos t) = addEdgesVariableT vert ignore t
addEdgesVariableT vert ignore (ShiftNeg v c) = addEdgesVariableC vert (v:ignore) c 

addEdgesVariablePt :: Vertex Variable -> [Variable] -> Pattern -> DepVar ()
addEdgesVariablePt vert ignore (MkPattern (MkXtorName nm) vars c) = do 
  addEdgesVariableC vert (vars ++ (MkVariable nm : ignore)) c

addEdgesVariableC :: Vertex Variable -> [Variable] -> Command -> DepVar ()
addEdgesVariableC vert ignore (Cut t _ u) = do
  addEdgesVariableT vert ignore t 
  addEdgesVariableT vert ignore u
addEdgesVariableC vert ignore (CutAnnot t _ _ u) = do
  addEdgesVariableT vert ignore t 
  addEdgesVariableT vert ignore u 
addEdgesVariableC _ _ Done = return ()

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
