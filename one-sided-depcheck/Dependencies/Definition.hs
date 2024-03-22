module Dependencies.Definition (
  DepM,
  runDepM,
  addEdgeM,
  addVertexM,
  getVertexError,
  ensureAcyclic,
  removeSelfLoops,
  DepError (..)
) where

import Dependencies.Graph
import Environment
import Common
import Errors 
import Loc
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except

data DepError = 
  ErrDuplModule !Modulename
  | ErrUndefinedModule !Modulename
  | ErrMutualRec !Modulename
  | ErrUndefinedVar !Variable

instance Error DepError where 
    getMessage (ErrDuplModule mn) = "Module " <> show mn <> " was defined multiple times"
    getMessage (ErrUndefinedModule mn) = "Module " <> show mn <> " was not defined"
    getMessage (ErrMutualRec mn) = "Mutual Recusrion in module " <> show mn
    getMessage (ErrUndefinedVar v) = "Variable " <> show v <> " was not defined"
    getLoc _ = defaultLoc
    toError _ _ = error " not implemented"
  

newtype DepM a b = DepM { getCheckM :: ReaderT Environment (StateT (Graph a) (Except DepError)) b }
  deriving newtype (Functor, Applicative, Monad, MonadError DepError, MonadState (Graph a), MonadReader Environment)

addVertexM :: Eq a => Ord a => a -> DepM a (Vertex a)
addVertexM a = do
  gr <- get
  let (newV,gr') = addVertex a gr
  modify (const gr')
  return newV

getVertexError :: Eq a => a -> DepError -> DepM a (Vertex a) 
getVertexError lb err = do 
  gr <- get 
  let mvert = getVertex lb gr 
  case mvert of 
    Nothing   -> throwError err 
    Just vert -> return vert

addEdgeM :: Eq a => (Vertex a, Vertex a) -> DepM a () 
addEdgeM e = modify (addEdge e)

removeSelfLoops :: Eq a => DepM a () 
removeSelfLoops = do 
  MkGraph verts edgs <- get
  let newEdgs = filter (\(MkEdge v1 v2) -> v1 /= v2) edgs
  let gr' = MkGraph verts newEdgs
  modify (const gr')

ensureAcyclic :: Eq a => DepError -> DepM a () 
ensureAcyclic err = do
  gr <- get 
  forM_ (grVerts gr) (\v -> traverseGraph v [] [])
  where 
    traverseGraph :: Eq a => Vertex a -> [Vertex a] -> [Edge a] -> DepM a ([Vertex a],[Edge a])
    traverseGraph startV seenV seenE = do 
      when (startV `elem` seenV) $ throwError err
      gr <- get 
      let outg = getEdgesStartingAt startV gr
      let outgF = filter (`notElem` seenE) outg
      let newSeenV = startV : seenV
      case outgF of 
        [] -> return (newSeenV,seenE)
        edgs -> foldM (\(seenV',seenE') edg@(MkEdge v1 v2) -> traverseGraph v2 (v1:seenV') (edg:seenE')) (newSeenV, seenE) edgs
    

runDepM :: Environment -> DepM a b -> Either DepError b
runDepM env m = case runExcept (runStateT (runReaderT (getCheckM m) env) emptyGraph) of 
  Left err -> Left err
  Right (x,_) -> Right x
