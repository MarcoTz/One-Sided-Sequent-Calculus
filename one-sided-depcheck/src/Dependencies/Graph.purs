module Dependencies.Graph (
  Edge (..),
  Vertex,
  Graph (..),
  emptyGraph,
  getEdgesStartingAt,
  getEdgesEndingAt,
  addEdge,
  addVertex,
  getStartingVert,
  getEndingVert,
  getVertex,
  getVertexLabel
) where

import Prelude (class Eq, class Ord, (==))
import Data.Set (Set, empty, insert)
import Data.List (List(..), find, elem, filter)
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..))

newtype Vertex a = MkVertex a
derive instance eqVertex :: Eq a => Eq (Vertex a)
derive instance ordVertex :: Ord a => Ord (Vertex a)

data Edge a = MkEdge   (Vertex a) (Vertex a)
derive instance eqEdge :: Eq a => Eq (Edge a)
derive instance ordEdge :: Ord a => Ord (Edge a)

data Graph  a = MkGraph {grVerts :: (Set (Vertex a)), grEdges :: List (Edge a)}

emptyGraph :: forall a.Graph a 
emptyGraph = MkGraph {grVerts:empty, grEdges:Nil}

addVertex :: forall a.Eq a => Ord a => a -> Graph a -> Tuple (Vertex a) (Graph a)
addVertex v gr@(MkGraph {grVerts:verts, grEdges:edges}) = 
  case getVertex v gr of 
    Just vert -> Tuple vert gr
    Nothing ->  
      let newVert = MkVertex v in
          Tuple newVert (MkGraph {grVerts:insert newVert verts,grEdges:edges})

getVertex :: forall a. Eq a => a -> Graph a -> Maybe (Vertex a)
getVertex lb (MkGraph {grVerts:verts, grEdges:_}) = find (\(MkVertex lb') -> lb == lb') verts 

getVertexLabel :: forall a. Vertex a -> a 
getVertexLabel (MkVertex l) = l

addEdge :: forall a.Eq a => Tuple (Vertex a) (Vertex a) -> Graph a -> Graph a
addEdge (Tuple a1 a2) gr@(MkGraph {grVerts:verts, grEdges:edges}) = 
  if MkEdge a1 a2 `elem` edges then gr else
  MkGraph {grVerts:verts, grEdges:(Cons (MkEdge a1 a2) edges)}

getStartingVert :: forall a.Edge a -> Vertex a
getStartingVert (MkEdge v _) = v

getEndingVert :: forall a.Edge a -> Vertex a 
getEndingVert (MkEdge _ v) = v

getEdgesStartingAt :: forall a.Eq a => Vertex a -> Graph a -> List (Edge a)
getEdgesStartingAt vert (MkGraph {grVerts:_, grEdges:edges}) = filter (\e -> getStartingVert e==vert) edges

getEdgesEndingAt :: forall a. Eq a => Vertex a -> Graph a -> List (Edge a)
getEdgesEndingAt vert (MkGraph {grVerts:_, grEdges:edges}) = filter (\e -> getEndingVert e==vert) edges
