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
import Dependencies.Errors
import Environment
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except  

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

removeSelfLoops :: Show a => Eq a => DepM a () 
removeSelfLoops = do 
  MkGraph verts edgs <- get
  let newEdgs = filter (\(MkEdge v1 v2) -> v1 /= v2) edgs
  let gr' = MkGraph verts newEdgs
  modify (const gr')

ensureAcyclic :: Show a => Eq a => Show a => DepError -> DepM a () 
ensureAcyclic err = do
  gr <- get 
  forM_ (grVerts gr) (\v -> traverseGr gr v [])
  where 
    traverseGr :: Show a => Eq a => Graph a -> Vertex a -> [Vertex a] -> DepM a [Vertex a]
    traverseGr gr startV seenV = do
      let newSeenV = startV : seenV
      let outg = getEndingVert <$> getEdgesStartingAt startV gr
      when (any (`elem` outg) newSeenV) $ throwError err
      forM_ outg (\v -> traverseGr gr v newSeenV)
      return []


runDepM :: Environment -> DepM a b -> Either DepError b
runDepM env m = case runExcept (runStateT (runReaderT (getCheckM m) env) emptyGraph) of 
  Left err -> Left err
  Right (x,_) -> Right x
