module Dependencies.Definition (
  DepM,
  runDepM,
  addEdgeM,
  addVertexM,
  getVertexError,
  ensureAcyclic,
  removeSelfLoops
) where

import Environment (Environment)
import Dependencies.Graph (Graph(..), Vertex, Edge(..), emptyGraph, addVertex, getVertex, addEdge, getEndingVert, getEdgesStartingAt)
import Dependencies.Errors (DepError)

import Prelude (class Eq, class Ord, class Show, bind, pure, const, (/=), (<$>), ($), identity)
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..))
import Data.List (List(..), filter,elem, null)
import Data.Either (Either(..))
import Data.Unit (Unit,unit)
import Data.Set (toUnfoldable)
import Data.Traversable (for)
import Control.Monad (when)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.State (StateT,gets,modify, runStateT)
import Control.Monad.Except (Except,throwError, runExcept)

type DepM a b = ReaderT Environment (StateT (Graph a) (Except DepError)) b 

runDepM :: forall a b. Environment -> DepM a b -> Either DepError b
runDepM env m = case runExcept (runStateT (runReaderT m env) emptyGraph) of 
  Left err -> Left err
  Right (Tuple x _) -> Right x

addVertexM :: forall a.Eq a => Ord a => a -> DepM a (Vertex a)
addVertexM a = do
  gr <- gets (\(MkGraph gr) -> gr)
  let (Tuple newV gr') = addVertex a (MkGraph gr)
  _ <- modify (const gr')
  pure newV

getVertexError :: forall a.Eq a => a -> DepError -> DepM a (Vertex a) 
getVertexError lb err = do 
  gr <- gets (\(MkGraph gr) -> gr)
  let mvert = getVertex lb (MkGraph gr)
  case mvert of 
    Nothing   -> throwError err 
    Just vert -> pure vert

addEdgeM :: forall a. Eq a => Tuple (Vertex a) (Vertex a) -> DepM a Unit 
addEdgeM e = do 
  _ <- modify (addEdge e)
  pure unit

removeSelfLoops :: forall a. Show a => Eq a => DepM a Unit 
removeSelfLoops = do 
  gr <- gets (\(MkGraph gr) -> gr)
  let newEdgs = filter (\(MkEdge v1 v2) -> v1 /= v2) gr.grEdges
  let gr' = MkGraph {grVerts:gr.grVerts, grEdges:newEdgs}
  _ <- modify (const gr')
  pure unit

ensureAcyclic :: forall a.Show a => Eq a => Show a => DepError -> DepM a Unit 
ensureAcyclic err = do
  gr <- gets (\(MkGraph gr) -> gr) 
  let verts :: List (Vertex a)
      verts = toUnfoldable (gr.grVerts)
  _ <- for verts (\v -> traverseGr (MkGraph gr) v Nil) 
  pure unit
  where 
    traverseGr :: Show a => Eq a => Graph a -> Vertex a -> List (Vertex a) -> DepM a (List (Vertex a))
    traverseGr gr startV seenV = do
      let newSeenV = Cons startV seenV
      let outg = getEndingVert <$> getEdgesStartingAt startV gr
      let elems = (\x -> (x `elem` outg)) <$> newSeenV
      _ <- when (null (filter identity elems))  $ throwError err
      _ <- for outg (\v -> traverseGr gr v newSeenV) 
      pure Nil
      
